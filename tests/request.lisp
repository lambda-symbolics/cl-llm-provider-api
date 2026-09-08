(in-package #:cl-llm-provider-api)

(defun test-request-callback-condition-identity ()
  "Keep callback conditions out of the transport retry classifier."
  (dolist (phase '(:delta :terminal :completion))
    (dolist (class '(usocket:socket-error usocket:ns-try-again-error
                     sb-sys:deadline-timeout simple-error))
      (let* ((failure (make-condition class))
             (attempts 0)
             (cleanups 0)
             (completions 0)
             (retry-events nil)
             (result
               (handler-case
                   (call-with-bounded-retries
                    (lambda ()
                      (incf attempts)
                      (provider--call-with-transport-normalization
                       (lambda ()
                         (provider-execute-request
                          (make-instance 'responses-api-provider) nil
                          :transport
                          (lambda (request)
                            (declare (ignore request))
                            (values
                             (make-string-input-stream
                              (concatenate
                               'string
                               (test-sse-event-string
                                (json-object "type" "response.output_text.delta"
                                             "delta" "hello"))
                               (test-sse-event-string
                                (json-object "type" "response.completed"
                                             "response" (json-object "id" "done")))))
                             200 nil))
                          :event-callback
                          (lambda (event)
                            (when (or (and (eq phase ':delta)
                                           (typep event 'assistant-delta-event))
                                      (and (eq phase ':terminal)
                                           (typep event 'provider-completed-event)))
                              (error failure)))
                          :cleanup
                          (lambda (stream)
                            (incf cleanups)
                            (provider--close-response-stream stream))
                          :completion
                          (lambda ()
                            (incf completions)
                            (when (eq phase ':completion) (error failure)))))))
                    (lambda (event) (push event retry-events))
                    :maximum-retries 1 :delay-function (constantly 0)
                    :sleep-function (constantly nil))
                 (condition (condition) condition))))
        (test-assert (eq result failure) "callback failures preserve condition identity")
        (test-assert (and (= attempts 1) (= cleanups 1) (null retry-events))
                     "callback failures do not retry the completed request")
        (test-assert (= completions (if (eq phase ':completion) 1 0))
                     "completion is only called after successful consumption")))))

(defun test-request-transport-conditions ()
  "Normalize opening and line-read failures at their transport boundaries."
  (dolist (phase '(:open :read))
    (dolist (case '((usocket:socket-error t)
                    (usocket:ns-try-again-error t)
                    (sb-sys:deadline-timeout t)
                    (cl+ssl::cl+ssl-error nil)))
      (destructuring-bind (class retryable-p) case
        (let* ((failure (make-condition class))
               (stream (make-string-input-stream ""))
               (*sse-read-line-function*
                 (lambda (stream) (declare (ignore stream)) (error failure)))
               (result
                 (handler-case
                     (provider--call-with-transport-normalization
                      (lambda ()
                        (provider-execute-request
                         (make-instance 'responses-api-provider) nil
                         :transport (lambda (request)
                                      (declare (ignore request))
                                      (when (eq phase ':open) (error failure))
                                      (values stream 200 nil))
                         :event-callback #'identity)))
                   (condition (condition) condition))))
          (unwind-protect
               (progn
                 (test-assert (and (typep result 'provider-error)
                                   (eq (not (null (typep result 'provider-retryable-error)))
                                       retryable-p))
                              "transport operations retain their failure classification")
                 (test-assert (eq (open-stream-p stream) (eq phase ':open))
                              "acquired streams are closed after read failures"))
            (provider--close-response-stream stream)))))))
(defun run-request-tests ()
  "Exercise transport injection, cleanup, deadline, and redaction boundaries."
  (test-request-callback-condition-identity)
  (test-request-transport-conditions)
  (dolist (case '((200 :complete) (200 :interrupted) (429 :failure)))
    (destructuring-bind (status terminal) case
      (let* ((secret (make-string 19 :initial-element #\k))
             (raw-headers (list (cons "x-request-id" secret)))
             (source (test-sse-event-string
                      (json-object "type" "response.completed"
                                   "response" (json-object "id" secret))))
             (stream (make-string-input-stream
                      (if (eq terminal ':complete) source "")))
             (order nil)
             (events nil)
             (request (json-object "model" "test"))
             (outcome
               (handler-case
                   (provider-execute-request
                    (make-instance 'responses-api-provider) request
                    :secrets (list secret)
                    :transport (lambda (received)
                                 (test-assert (eq request received)
                                              "transport receives the request object")
                                 (values stream status raw-headers))
                    :event-callback (lambda (event) (push event events))
                    :call-with-deadline (lambda (function)
                                          (push :deadline order)
                                          (funcall function))
                    :cleanup (lambda (stream)
                               (push :cleanup order)
                               (provider--close-response-stream stream))
                    :completion (lambda () (push :completion order)))
                 (provider-error (condition) condition))))
        (test-assert (not (open-stream-p stream)) "every response is closed")
        (test-assert (string= (cdar raw-headers) secret)
                     "sanitization does not mutate transport headers")
        (case terminal
          (:complete
           (test-assert (equal (reverse order) '(:deadline :cleanup :completion))
                        "completion follows deadline-wrapped consumption and cleanup")
           (test-assert (and (typep outcome 'provider-result)
                             (not (search secret (provider-result-response-id outcome))))
                        "semantic results contain no credential echo")
           (test-assert (not (search secret
                                    (provider-completed-event-response-id (first events))))
                        "callbacks receive redacted semantic events"))
          (:interrupted
           (test-assert (and (typep outcome 'response-stream-error)
                             (equal (reverse order) '(:deadline :cleanup)))
                        "interrupted streams clean up without completing"))
          (:failure
           (test-assert (and (typep outcome 'provider-retryable-error)
                             (= (provider-error-status outcome) 429)
                             (not (search secret (provider-error-request-id outcome)))
                             (equal order '(:cleanup)))
                        "HTTP failure classification precedes consumption"))))))
  (dolist (terminal-errors-p '(nil t))
    (let* ((failure (make-condition 'simple-error :format-control "request failed"))
           (result
             (handler-case
                 (provider--call-with-transport-normalization
                  (lambda () (error failure)) :terminal-errors-p terminal-errors-p)
               (error (condition) condition))))
      (test-assert
       (if terminal-errors-p
           (and (typep result 'provider-error)
                (not (typep result 'provider-retryable-error)))
           (eq failure result))
       "opening failures are terminal; application callback errors keep their identity")))
  (let ((attempts 0) (wrappers 0) (delays nil) (events nil))
    (test-assert
     (eq :complete
         (call-with-bounded-retries
          (lambda ()
            (case (incf attempts)
              (1 (error 'provider-resample-requested :message "resample"
                                                   :attempt 1 :maximum-attempts 2))
              (2 (error 'provider-retryable-error :message "transient"))
              (otherwise :complete)))
          (lambda (event) (push event events))
          :maximum-retries 1
          :delay-function (lambda (number condition)
                            (test-assert (typep condition 'provider-retryable-error)
                                         "delay policy receives the failure")
                            (* number 3))
          :sleep-function (lambda (delay) (push delay delays))
          :call-with-attempt (lambda (function) (incf wrappers) (funcall function))))
     "retry and resampling succeed within separate limits")
    (test-assert (and (= attempts 3) (= wrappers 3) (equal delays '(3))
                      (equal (mapcar #'provider-retry-event-delay (reverse events))
                             '(0 3 0)))
                 "resampling consumes no transient retry and invokes no delay")))
