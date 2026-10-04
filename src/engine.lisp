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

(define-condition provider-error (provider-api-error)
  ((status :initarg :status :initform nil :reader provider-error-status
           :type (or null integer)
           :documentation "The HTTP status, if a response was received.")
   (code :initarg :code :initform nil :reader provider-error-code
         :type (or null string)
         :documentation "The provider's structured error code, if supplied.")
   (request-id :initarg :request-id :initform nil :reader provider-error-request-id
               :type (or null string)
               :documentation "The request identifier, if supplied.")
   (response-id :initarg :response-id :initform nil :reader provider-error-response-id
                :type (or null string)
                :documentation "The failed response identifier, if supplied.")
   (response :initarg :response :initform nil :reader provider-error-response
             :type (or null string)
             :documentation "A bounded provider response safe for display."))
  (:documentation "A model-provider request failed."))

(define-condition provider-retryable-error (provider-error)
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

(define-condition provider-stream-abandoned (provider-error)
  ((attempts
    :initarg :attempts
    :reader provider-stream-abandoned-attempts
    :type (integer 1)
    :documentation "How many attempts had streamed output before giving up."))
  (:documentation
   "A request kept failing after streaming output, past the streaming retry budget."))

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

(defparameter *provider-transport-operation-wrapper*
  (lambda (function &key terminal-errors-p)
    (declare (ignore terminal-errors-p))
    (funcall function))
  "Request-local adapter for actual transport opening and SSE line reads.
The wrapper accepts a thunk and :TERMINAL-ERRORS-P, true only for opening.
Application callbacks execute outside this boundary.")

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
        (let ((raw-line
                (funcall *provider-transport-operation-wrapper*
                         (lambda () (funcall *sse-read-line-function* stream)))))
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


(defun call-with-bounded-retries
       (attempt-function event-callback
        &key (delays *bounded-retry-delays*) (maximum-retries (length delays))
        delay-function (sleep-function *bounded-retry-sleep-function*)
        (call-with-attempt #'funcall))
  "Call ATTEMPT-FUNCTION with bounded transient retry and independent resampling. DELAY-FUNCTION receives the one-based retry number and condition. CALL-WITH-ATTEMPT wraps each attempt for host deadlines or scoped resources."
  (let ((retry-number 0))
    (loop
     (handler-case (return (funcall call-with-attempt attempt-function))
                   (provider-resample-requested (condition)
                    (funcall event-callback
                             (make-instance 'provider-retry-event :attempt
                                            (provider-resample-requested-attempt
                                             condition)
                                            :maximum-attempts
                                            (provider-resample-requested-maximum-attempts
                                             condition)
                                            :delay 0)))
                   (provider-retryable-error (condition)
                    (when (>= retry-number maximum-retries) (error condition))
                    (incf retry-number)
                    (let ((delay
                           (if delay-function
                               (funcall delay-function retry-number condition)
                               (nth (1- retry-number) delays))))
                      (funcall event-callback
                               (make-instance 'provider-retry-event :attempt
                                              retry-number :maximum-attempts
                                              maximum-retries :delay delay))
                      (funcall sleep-function delay)
                      (funcall event-callback
                               (make-instance 'provider-retry-event :attempt
                                              retry-number :maximum-attempts
                                              maximum-retries :delay 0))))))))


;;;; -- Streaming-Aware Retries --

(defparameter *provider-maximum-transient-retries* 6
  "Maximum retryable failures allowed after the initial attempt.")

(defparameter *provider-maximum-streaming-retries* 2
  "Maximum retries of one request after an attempt already streamed model output.

A failure before any output costs only the wait, so the full transient ladder
applies. Once reasoning or output has streamed, every retry bills a fresh
generation of the same prompt, so the budget is deliberately tighter.")

(defun provider-jittered-retry-delay (retry-number &key (random-state *random-state*))
  "Return the seconds to wait before one-based retry RETRY-NUMBER.

The base doubles from one second up to 32, and a uniform factor from 0.8 to 1.2
spreads simultaneous clients apart. The result is a whole number of seconds from
1 to 60."
  (let ((base (min 50 (ash 1 (min 6 (1- retry-number))))))
    (max 1 (min 60 (round (* base (+ 0.8d0 (random 0.4d0 random-state))))))))

(defun call-with-streaming-retries
    (attempt-function event-callback
     &key (maximum-retries *provider-maximum-transient-retries*)
          (maximum-streaming-retries *provider-maximum-streaming-retries*)
          (sleep-function *bounded-retry-sleep-function*)
          (random-state *random-state*)
          delay-function
          (call-with-attempt #'funcall))
  "Call ATTEMPT-FUNCTION with bounded retries that tighten once output has streamed.

ATTEMPT-FUNCTION receives the event callback each attempt must stream through,
so streamed reasoning, text and items are observed. Every failed attempt is
reported to EVENT-CALLBACK as a PROVIDER-ATTEMPT-FAILED-EVENT before the
ladder decides. Failures before any output may retry MAXIMUM-RETRIES times;
retryable failures after output streamed are capped at
MAXIMUM-STREAMING-RETRIES, after which the request ends with
PROVIDER-STREAM-ABANDONED. DELAY-FUNCTION defaults to
PROVIDER-JITTERED-RETRY-DELAY drawing from RANDOM-STATE; SLEEP-FUNCTION and
CALL-WITH-ATTEMPT are passed to CALL-WITH-BOUNDED-RETRIES."
  (let ((attempt-number 0)
        (streaming-failures 0)
        (output-received-p nil)
        (started-at 0))
    (labels ((observe-event (event)
               "Note streamed output before forwarding EVENT."
               (when (typep event '(or assistant-delta-event
                                       reasoning-delta-event
                                       provider-item-event))
                 (setf output-received-p t))
               (funcall event-callback event))

             (elapsed-seconds ()
               "Return whole seconds since the current attempt started."
               (max 0 (round (- (get-internal-real-time) started-at)
                             internal-time-units-per-second)))

             (note-failure (condition)
               "Report CONDITION and enforce the streaming retry budget."
               (let ((retryable-p (typep condition 'provider-retryable-error)))
                 (funcall event-callback
                          (make-instance 'provider-attempt-failed-event
                                         :attempt attempt-number
                                         :elapsed-seconds (elapsed-seconds)
                                         :output-received-p output-received-p
                                         :retryable-p retryable-p
                                         :condition condition))
                 (when (and retryable-p output-received-p)
                   (incf streaming-failures)
                   (when (> streaming-failures maximum-streaming-retries)
                     (error 'provider-stream-abandoned
                            :message
                            (format nil
                                    "The provider stream failed after model output began on ~D attempts; giving up instead of billing another generation. Last failure: ~A"
                                    streaming-failures condition)
                            :status (provider-error-status condition)
                            :code (provider-error-code condition)
                            :request-id (provider-error-request-id condition)
                            :response-id (provider-error-response-id condition)
                            :response (provider-error-response condition)
                            :attempts streaming-failures)))))

             (attempt ()
               "Run one attempt with fresh output tracking."
               (incf attempt-number)
               (setf output-received-p nil
                     started-at (get-internal-real-time))
               (handler-bind ((provider-error
                                (lambda (condition)
                                  (unless (typep condition 'provider-resample-requested)
                                    (note-failure condition)))))
                 (funcall attempt-function #'observe-event))))
      (call-with-bounded-retries
       #'attempt #'observe-event
       :maximum-retries maximum-retries
       :sleep-function sleep-function
       :call-with-attempt call-with-attempt
       :delay-function (or delay-function
                           (lambda (retry-number condition)
                             (declare (ignore condition))
                             (provider-jittered-retry-delay retry-number
                                                            :random-state random-state)))))))
