(in-package #:cl-llm-provider-api/tests)

;;;; -- Streaming Retry Tests --

(defun retry-tests--stream-failure ()
  "Return a retryable stream failure carrying request identifiers."
  (make-condition 'response-stream-error
                  :message "Injected stream failure."
                  :status nil :code nil :request-id "req-stream"
                  :response-id "resp-stream" :response nil))

(defun retry-tests--no-sleep (delay)
  "Skip a retry wait."
  (declare (ignore delay))
  nil)

(defun test-streaming-retry-budget ()
  "Exercise the tighter budget once output streamed and the full cold ladder."
  (let ((attempts 0)
        (events nil)
        (outcome nil))
    (handler-case
        (call-with-streaming-retries
         (lambda (event-callback)
           (incf attempts)
           (funcall event-callback (make-instance 'assistant-delta-event :text "partial"))
           (error (retry-tests--stream-failure)))
         (lambda (event) (push event events))
         :maximum-retries 6 :maximum-streaming-retries 2 :sleep-function #'retry-tests--no-sleep)
      (provider-stream-abandoned (condition)
        (setf outcome condition)))
    (setf events (nreverse events))
    (check (and outcome (= attempts 3)) "the third failure after streamed output abandons the request")
    (check (and (= (provider-stream-abandoned-attempts outcome) 3)
                (string= (provider-error-request-id outcome) "req-stream")
                (string= (provider-error-response-id outcome) "resp-stream")
                (not (typep outcome 'provider-retryable-error)))
           "the abandoned stream keeps the last failure's identifiers and is terminal")
    (let ((failures (remove-if-not (lambda (event) (typep event 'provider-attempt-failed-event))
                                   events)))
      (check (and (equal (mapcar #'provider-attempt-failed-event-attempt failures) '(1 2 3))
                  (every #'provider-attempt-failed-event-output-received-p failures)
                  (every #'provider-attempt-failed-event-retryable-p failures))
             "every streamed attempt reports its failure with output received"))
    (check (= (count-if (lambda (event) (typep event 'provider-retry-event)) events) 4)
           "only the two permitted streaming retries produce reconnect events"))
  (let ((attempts 0)
        (events nil))
    (check (eq :ok (call-with-streaming-retries
                    (lambda (event-callback)
                      (declare (ignore event-callback))
                      (if (<= (incf attempts) 5)
                          (error (retry-tests--stream-failure))
                          :ok))
                    (lambda (event) (push event events))
                    :maximum-retries 6 :maximum-streaming-retries 2
                    :sleep-function #'retry-tests--no-sleep))
           "failures before any output use the full ladder")
    (check (notany #'provider-attempt-failed-event-output-received-p
                   (remove-if-not (lambda (event) (typep event 'provider-attempt-failed-event))
                                  events))
           "cold failures report no streamed output"))
  (let ((attempts 0))
    (check (eq :ok (call-with-streaming-retries
                    (lambda (event-callback)
                      (incf attempts)
                      (cond ((= attempts 1)
                             (funcall event-callback
                                      (make-instance 'reasoning-delta-event :text "think"))
                             (error (retry-tests--stream-failure)))
                            ((<= attempts 4)
                             (error (retry-tests--stream-failure)))
                            (t
                             :ok)))
                    (lambda (event) (declare (ignore event)) nil)
                    :maximum-retries 6 :maximum-streaming-retries 1
                    :sleep-function #'retry-tests--no-sleep))
           "output on one attempt does not taint later cold attempts"))
  (let ((sleeps nil)
        (events nil)
        (attempts 0))
    (handler-case (call-with-streaming-retries
                   (lambda (event-callback)
                     (declare (ignore event-callback))
                     (incf attempts)
                     (error (retry-tests--stream-failure)))
                   (lambda (event) (push event events))
                   :maximum-retries 2 :sleep-function (lambda (delay) (push delay sleeps))
                   :random-state (make-random-state t))
      (response-stream-error ()
        nil))
    (check (and (= attempts 3) (= (length sleeps) 2)
                (every (lambda (delay) (<= 1 delay 60)) sleeps))
           "exhaustion makes every permitted retry with bounded jittered waits"))
  (let ((events nil))
    (check (handler-case
               (call-with-streaming-retries
                (lambda (event-callback)
                  (declare (ignore event-callback))
                  (error 'provider-error :message "terminal" :status 400 :request-id nil
                                         :response nil))
                (lambda (event) (push event events)))
             (provider-error (condition)
               (not (typep condition 'provider-retryable-error))))
           "a terminal failure is not retried")
    (check (and (= (length events) 1)
                (not (provider-attempt-failed-event-retryable-p (first events))))
           "a terminal failure reports one attempt"))
  (let ((state (make-random-state t)))
    (check (every (lambda (retry)
                    (let ((delay (provider-jittered-retry-delay retry :random-state state))
                          (base (min 50 (ash 1 (min 6 (1- retry))))))
                      (and (<= 1 delay 60)
                           (<= (floor (* 0.8 base)) delay (ceiling (* 1.2 base))))))
                  '(1 2 3 4 5 6 7 8 20))
           "jittered delays stay within twenty percent of the doubling base")))

(defun run-retry-tests ()
  "Run the streaming retry, credential refresh and inactivity tests."
  (test-streaming-retry-budget))
