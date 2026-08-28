(in-package #:cl-llm-provider-api)

;;;; -- Engine Conditions --

(define-condition provider-api-error (error)
  ((message
    :initarg :message
    :reader provider-api-error-message
    :type string
    :documentation "The human-readable description of the failure."))
  (:report
   (lambda (condition stream)
     (format stream "~A" (provider-api-error-message condition))))
  (:documentation "A provider engine operation failed."))

(define-condition provider-retryable-error (provider-api-error)
  ()
  (:documentation "A transient provider failure eligible for bounded retry."))

(define-condition provider-resample-requested (provider-retryable-error)
  ((triggers
    :initarg :triggers
    :initform nil
    :reader provider-resample-requested-triggers
    :type list
    :documentation "The provider's degenerate-generation trigger labels.")
   (attempt
    :initarg :attempt
    :reader provider-resample-requested-attempt
    :type (integer 1)
    :documentation "The one-based resample attempt about to begin.")
   (maximum-attempts
    :initarg :maximum-attempts
    :reader provider-resample-requested-maximum-attempts
    :type (integer 1)
    :documentation "The provider's per-turn resample budget."))
  (:documentation
   "A provider reported a degenerate generation loop worth resampling."))

(define-condition provider-stream-limit-error (provider-retryable-error)
  ()
  (:documentation "A provider stream exceeded a configured size limit."))

(defparameter *stream-limit-error-class* 'provider-stream-limit-error
  "The condition class signaled for provider stream size violations.
Hosts may name a subclass carrying their own condition protocol.")


;;;; -- Bounded Character Reads --

(defparameter *character-read-sequence-window* 256
  "The largest single READ-SEQUENCE character request the engine issues.

SBCL 2.6.7 introduced SIMD utf-8 decoding that can overrun destination
strings when one request asks for more than 256 characters at once. Requests
at or below 256 characters stay on the portable buffered path on every
supported runtime.")

(defun read-character-sequence (buffer stream)
  "Fill BUFFER from STREAM like READ-SEQUENCE using bounded requests."
  (let ((length (length buffer))
        (filled 0))
    (loop
      (when (= filled length)
        (return filled))
      (let ((position
              (read-sequence buffer stream
                             :start filled
                             :end (min length
                                       (+ filled
                                          *character-read-sequence-window*)))))
        (when (= position filled)
          (return filled))
        (setf filled position)))))


;;;; -- SSE Decoding --

(defvar *sse-end-of-stream* '#:sse-end-of-stream
  "A private marker returned after a clean SSE end of stream.")

(defparameter *sse-maximum-line-characters* (* 1024 1024)
  "Maximum accepted character count for one SSE wire line.")

(defparameter *sse-maximum-event-characters* (* 4 1024 1024)
  "Maximum accepted joined data character count for one SSE event.")

(defun sse--signal-size-error (message)
  "Signal a bounded provider stream failure described by MESSAGE."
  (error *stream-limit-error-class* :message message))

(defun sse-data-line (line)
  "Return the payload of an SSE data LINE, or NIL for another field."
  (when (and (>= (length line) 5)
             (string= line "data:" :end1 5 :end2 5))
    (let ((start (if (and (> (length line) 5)
                          (char= (char line 5) #\Space))
                     6
                     5)))
      (subseq line start))))

(defun sse-read-line-characters (stream)
  "Read one bounded line using only the portable character-stream protocol."
  (let ((characters
          (make-array 256
                      :element-type 'character
                      :adjustable t
                      :fill-pointer 0)))
    (loop for character = (read-char stream nil *sse-end-of-stream*)
          do (cond
               ((eq character *sse-end-of-stream*)
                (return (if (plusp (length characters))
                            (coerce characters 'string)
                            *sse-end-of-stream*)))
               ((char= character #\Newline)
                (return (coerce characters 'string)))
               ((>= (length characters) *sse-maximum-line-characters*)
                (sse--signal-size-error
                 "The provider returned an SSE line above the configured limit."))
               (t
                (vector-push-extend character characters))))))

(defparameter *sse-read-line-function* #'sse-read-line-characters
  "The bounded line reader used by READ-SSE-DATA.
Hosts may install a wrapper adding runtime-specific inactivity deadlines.")

(defun read-sse-data (stream)
  "Read one bounded SSE event's joined data field from STREAM."
  (let ((data-stream (make-string-output-stream))
        (data-character-count 0)
        (data-line-count 0))
    (labels ((event-data ()
               (if (plusp data-line-count)
                   (get-output-stream-string data-stream)
                   *sse-end-of-stream*)))
      (loop
        (let ((raw-line (funcall *sse-read-line-function* stream)))
          (when (eq raw-line *sse-end-of-stream*)
            (return (event-data)))
          (let ((line (string-right-trim '(#\Return) raw-line)))
            (when (zerop (length line))
              (when (plusp data-line-count)
                (return (event-data))))
            (let ((data (sse-data-line line)))
              (when data
                (let ((next-count (+ data-character-count
                                     (if (plusp data-line-count) 1 0)
                                     (length data))))
                  (when (> next-count *sse-maximum-event-characters*)
                    (sse--signal-size-error
                     "The provider returned an SSE event above the configured limit."))
                  (when (plusp data-line-count)
                    (write-char #\Newline data-stream))
                  (write-string data data-stream)
                  (setf data-character-count next-count)
                  (incf data-line-count))))))))))


;;;; -- Bounded Retries --

(defparameter *bounded-retry-delays* '(1 2 4 8 16)
  "Backoff seconds for bounded provider request reconnects.")

(defparameter *bounded-retry-sleep-function* #'sleep
  "Function used to wait between provider retry attempts.")

(defun call-with-bounded-retries (attempt-function event-callback
                                  &key (delays *bounded-retry-delays*)
                                       (sleep-function
                                        *bounded-retry-sleep-function*))
  "Call ATTEMPT-FUNCTION of no arguments with bounded transport recovery.

PROVIDER-RESAMPLE-REQUESTED restarts the attempt immediately, because a
fresh sample is the remedy for a stochastic degenerate generation and the
signaling provider enforces its own per-turn resample budget. Any other
PROVIDER-RETRYABLE-ERROR waits for the next entry in DELAYS before the
attempt is repeated, and propagates once DELAYS is exhausted.
EVENT-CALLBACK receives one PROVIDER-RETRY-EVENT before each wait and
another with a zero delay when the wait ends, so a stalled reconnect never
reads as a frozen countdown."
  (let ((retry-number 0))
    (loop
      (handler-case
          (return-from call-with-bounded-retries
            (funcall attempt-function))
        (provider-resample-requested (condition)
          (funcall event-callback
                   (make-instance
                    'provider-retry-event
                    :attempt (provider-resample-requested-attempt condition)
                    :maximum-attempts
                    (provider-resample-requested-maximum-attempts condition)
                    :delay 0)))
        (provider-retryable-error (condition)
          (when (= retry-number (length delays))
            (error condition))
          (let ((delay (nth retry-number delays)))
            (funcall event-callback
                     (make-instance
                      'provider-retry-event
                      :attempt (1+ retry-number)
                      :maximum-attempts (length delays)
                      :delay delay))
            (funcall sleep-function delay)
            (funcall event-callback
                     (make-instance
                      'provider-retry-event
                      :attempt (1+ retry-number)
                      :maximum-attempts (length delays)
                      :delay 0))
            (incf retry-number)))))))
