(in-package #:cl-llm-provider-api)

(defun run-request-tests ()
  "Exercise transport injection, cleanup, deadline, and redaction boundaries."
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
