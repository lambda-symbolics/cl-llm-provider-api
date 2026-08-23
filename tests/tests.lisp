(defpackage #:cl-llm-provider-api/tests
  (:use #:cl #:cl-llm-provider-api)
  (:export #:run-tests))

(in-package #:cl-llm-provider-api/tests)

(defvar *assertions* 0)

(defun check (value control &rest arguments)
  "Count one assertion and fail with CONTROL unless VALUE is true."
  (incf *assertions*)
  (unless value
    (error (apply #'format nil control arguments))))

(defclass test-responses-provider (responses-api-provider)
  ())

(defmethod provider-stream-turn
    ((provider test-responses-provider) conversation
     &key tool-namespaces event-callback goal-context compaction-p)
  (declare (ignore provider tool-namespaces goal-context compaction-p))
  (when event-callback
    (funcall event-callback
             (make-instance 'assistant-delta-event :text "hello")))
  (make-instance 'provider-result
                 :response-id "response-1"
                 :output-items (list conversation)
                 :usage '(:total-tokens 7)
                 :turn-completion :end))

(defun test-provider-values ()
  "Exercise portable provider events and results."
  (let ((delta (make-instance 'assistant-delta-event :text "part"))
        (reasoning (make-instance 'reasoning-delta-event :text "summary"))
        (item (make-instance 'provider-item-event :item '(:kind :message)))
        (completed (make-instance 'provider-completed-event
                                  :response-id "r"
                                  :usage '(:tokens 4)
                                  :turn-completion :continue))
        (retry (make-instance 'provider-retry-event
                              :attempt 2
                              :maximum-attempts 4
                              :delay 0.5)))
    (check (typep delta 'provider-event) "delta is not a provider event")
    (check (string= (assistant-delta-event-text delta) "part")
           "assistant delta lost its text")
    (check (string= (reasoning-delta-event-text reasoning) "summary")
           "reasoning delta lost its text")
    (check (equal (provider-item-event-item item) '(:kind :message))
           "item event lost its value")
    (check (and (string= (provider-completed-event-response-id completed) "r")
                (equal (provider-completed-event-usage completed) '(:tokens 4))
                (eq (provider-completed-event-turn-completion completed) :continue))
           "completed event lost terminal metadata")
    (check (and (= (provider-retry-event-attempt retry) 2)
                (= (provider-retry-event-maximum-attempts retry) 4)
                (= (provider-retry-event-delay retry) 0.5))
           "retry event lost reconnect metadata")))

(defun test-provider-protocol ()
  "Exercise generic provider and wire protocol defaults."
  (let* ((provider (make-instance 'test-responses-provider))
         (events nil)
         (result (provider-stream-turn
                  provider '(:role :user)
                  :event-callback (lambda (event) (push event events)))))
    (check (eq (provider-wire-protocol provider) :responses-api)
           "Responses provider has the wrong wire family")
    (check (string= (provider-wire-tool-name provider "resource" "read")
                    "resource.read")
           "Responses tool names are not flattened")
    (check (equal (provider-result-output-items result) '((:role :user)))
           "provider result lost output items")
    (check (equal (provider-result-usage result) '(:total-tokens 7))
           "provider result lost usage")
    (check (eq (provider-result-turn-completion result) :end)
           "provider result lost completion state")
    (check (and (= (length events) 1)
                (string= (assistant-delta-event-text (first events)) "hello"))
           "stream callback did not receive the semantic event")
    (check (null (provider-native-compact-conversation provider nil))
           "native compaction default is not NIL")
    (check (null (provider-output-ceiling-p provider))
           "output ceiling default is not conservative")
    (check (eq (provider-set-reasoning-summaries provider t) provider)
           "reasoning summary default did not preserve provider")
    (check (eq (provider-normalize-output-item provider result) result)
           "normalization default changed the item")
    (check (equalp (provider-responses-request-namespaces provider #(a b)) #(a b))
           "Responses namespace default changed input")
    (check (null (provider-responses-hosted-tools provider nil))
           "hosted tools default is not NIL")
    (check (null (provider-responses-reasoning-summary provider nil))
           "reasoning summary style default is not NIL")
    (check (null (provider-responses-request-fields provider nil))
           "Responses request fields default is not NIL")))

(defun run-tests ()
  "Run every cl-llm-provider-api test."
  (setf *assertions* 0)
  (test-provider-values)
  (test-provider-protocol)
  (format t "~&~D cl-llm-provider-api tests passed.~%" *assertions*)
  t)
