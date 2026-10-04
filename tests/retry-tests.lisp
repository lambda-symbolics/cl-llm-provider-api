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

(defun test-credential-refresh ()
  "Exercise one forced refresh after a rejection and the exhausted hook."
  (flet ((rejection ()
           (make-condition 'provider-unauthorized :message "rejected" :status 401
                                                  :request-id nil :response nil)))
    (let ((refreshes nil))
      (check (eq :ok (call-with-credential-refresh
                      (lambda (force-refresh-p)
                        (push force-refresh-p refreshes)
                        (if force-refresh-p :ok (error (rejection))))
                      :refreshable-p t))
             "a refreshable rejection retries once with a forced refresh")
      (check (equal refreshes '(t nil)) "the second attempt forces the refresh"))
    (let ((attempts 0))
      (check (eq :host (handler-case
                           (call-with-credential-refresh
                            (lambda (force-refresh-p)
                              (declare (ignore force-refresh-p))
                              (incf attempts)
                              (error (rejection)))
                            :refreshable-p t
                            :exhausted-function (lambda (condition)
                                                  (declare (ignore condition))
                                                  (error "host authentication failure")))
                         (simple-error () :host)))
             "the exhausted hook signals the host's own condition")
      (check (= attempts 2) "a refreshable request makes exactly two attempts"))
    (let ((attempts 0))
      (check (handler-case
                 (call-with-credential-refresh (lambda (force-refresh-p)
                                                 (declare (ignore force-refresh-p))
                                                 (incf attempts)
                                                 (error (rejection))))
               (provider-unauthorized () t))
             "without refresh the rejection is resignaled")
      (check (= attempts 1) "a non-refreshable request makes one attempt"))))

(defun retry-tests--connected-stream ()
  "Return a loopback character stream and the sockets that hold it open."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener (sb-bsd-sockets:make-inet-address "127.0.0.1") 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (multiple-value-bind (address port) (sb-bsd-sockets:socket-name listener)
      (let ((client (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
        (sb-bsd-sockets:socket-connect client address port)
        (values (sb-bsd-sockets:socket-make-stream client :input t :output t
                                                          :element-type 'character)
                (list client (sb-bsd-sockets:socket-accept listener) listener))))))

(defun test-sse-inactivity-deadline ()
  "Exercise the per-line SSE stall deadline."
  (multiple-value-bind (stream sockets) (retry-tests--connected-stream)
    (unwind-protect
         (let ((*sse-inactivity-seconds* 1)
               (started (get-universal-time)))
           (check (handler-case (progn (sse-read-line-within-inactivity-deadline stream) nil)
                    (response-stream-error (condition)
                      (search "delivered nothing" (provider-api-error-message condition))))
                  "a stalled stream signals a retryable stream failure")
           (check (< (- (get-universal-time) started) 30) "the stall ends on its own deadline")
           (let ((peer (sb-bsd-sockets:socket-make-stream (second sockets) :input t :output t
                                                                           :element-type 'character)))
             (write-line "data: delivered" peer)
             (finish-output peer)
             (check (string= (sse-read-line-within-inactivity-deadline stream) "data: delivered")
                    "a delivered line is returned within the deadline")))
      (ignore-errors (close stream))
      (dolist (socket sockets)
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))

(defun test-event-stream-post ()
  "Exercise posting an encoded body and receiving the event stream unbuffered."
  (let ((listener (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
        (received nil))
    (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
    (sb-bsd-sockets:socket-bind listener (sb-bsd-sockets:make-inet-address "127.0.0.1") 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name listener)))
           (server
             (bt:make-thread
              (lambda ()
                (let* ((socket (sb-bsd-sockets:socket-accept listener))
                       (stream (sb-bsd-sockets:socket-make-stream
                                socket :input t :output t :element-type :default
                                        :external-format :latin-1)))
                  (unwind-protect
                       (let ((length 0))
                         (loop for line = (string-right-trim '(#\Return) (read-line stream))
                               until (zerop (length line))
                               do (push line received)
                                  (when (eql 0 (search "content-length:" (string-downcase line)))
                                    (setf length (parse-integer line :start 15))))
                         (let ((body (make-string length)))
                           (read-sequence body stream)
                           (push body received))
                         (format stream "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CConnection: close~C~C~C~Cdata: hello~%~%"
                                 #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                 #\Return #\Newline)
                         (finish-output stream))
                    (ignore-errors (close stream))
                    (ignore-errors (sb-bsd-sockets:socket-close socket)))))
              :name "event stream test server")))
      (unwind-protect
           (multiple-value-bind (body status)
               (provider-post-event-stream (format nil "http://127.0.0.1:~D/stream" port)
                                           "{\"stream\":true}"
                                           :headers '(("Accept" . "text/event-stream"))
                                           :deadline-seconds 10)
             (unwind-protect
                  (progn
                    (check (= status 200) "the post returns the response status")
                    (check (string= (read-line body) "data: hello")
                           "the response body is an unbuffered event stream"))
               (ignore-errors (close body))))
        (bt:join-thread server)
        (ignore-errors (sb-bsd-sockets:socket-close listener))))
    (check (and (string= (first received) "{\"stream\":true}")
                (find-if (lambda (line) (eql 0 (search "POST /stream" line))) received)
                (find "Accept: text/event-stream" received :test #'string-equal))
           "the encoded body and headers reach the server")))

(defun run-retry-tests ()
  "Run the streaming retry, credential refresh and inactivity tests."
  (test-streaming-retry-budget)
  (test-credential-refresh)
  (test-sse-inactivity-deadline)
  (test-event-stream-post))
