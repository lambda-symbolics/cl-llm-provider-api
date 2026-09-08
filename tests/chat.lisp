(in-package #:cl-llm-provider-api)


(defun openai-compatible-provider-tests--stream-event
       (delta response-id &optional finish-reason)
  "Return one Chat Completions stream event containing DELTA."
  (json-object "id" response-id "choices"
   (json-array (json-object "delta" delta "finish_reason" finish-reason))))

(defun openai-compatible-provider-tests--terminal-semantics (provider)
  "Test Chat Completions terminal and structured-error outcomes by case."
  (labels ((consume (source)
             "Consume SOURCE and return either its result or typed condition."
             (handler-case
              (provider-consume-stream provider (make-string-input-stream source)
                                       '(("x-request-id" . "chat-terminal-request"))
                                       #'identity)
              (provider-error (condition) condition)))
           (finish-source (reason &key (done-p t))
             "Return a Chat stream with REASON followed by usage and optional DONE."
             (concatenate 'string
                          (test-sse-event-string
                           (openai-compatible-provider-tests--stream-event
                            (json-object "content" "partial")
                            "chat-terminal-response"))
                          (test-sse-event-string
                           (openai-compatible-provider-tests--stream-event
                            (json-object) "chat-terminal-response" reason))
                          (test-sse-event-string
                           (json-object "id" "chat-terminal-response" "choices"
                            (json-array) "usage"
                            (json-object "prompt_tokens" 4 "completion_tokens" 2
                             "total_tokens" 6)))
                          (if done-p
                              (format nil "data: [DONE]~%~%")
                              ""))))
    (dolist
        (case
         `(("normal completion" ,(finish-source "stop") provider-result)
           ("output truncation" ,(finish-source "length") provider-incomplete-response)
           ("unknown finish reason" ,(finish-source "future_reason")
            provider-protocol-error)
           ("[DONE] before terminal"
            ,(concatenate 'string
                          (test-sse-event-string
                           (openai-compatible-provider-tests--stream-event
                            (json-object "content" "partial")
                            "chat-terminal-response"))
                          (format nil "data: [DONE]~%~%"))
            response-stream-error)
           ("clean EOF before terminal"
            ,(test-sse-event-string
              (openai-compatible-provider-tests--stream-event
               (json-object "content" "partial") "chat-terminal-response"))
            response-stream-error)
           ("clean EOF after finish reason" ,(finish-source "stop" :done-p nil)
            response-stream-error)
           ("permanent structured error"
            ,(test-sse-event-string
              (json-object "error"
               (json-object "code" "invalid_request_error" "message"
                "invalid request")))
            provider-error)
           ("transient structured error"
            ,(test-sse-event-string
              (json-object "error"
               (json-object "code" "server_error" "message" "try again")))
            provider-retryable-error)))
      (destructuring-bind
          (name source expected-type)
          case
        (let ((outcome (consume source)))
          (test-assert (typep outcome expected-type)
           (format nil "Chat Completions ~A yields ~A" name expected-type))
          (when (typep outcome 'provider-incomplete-response)
            (test-assert
             (and (string= (provider-incomplete-response-reason outcome) "length")
                  (string= (provider-error-request-id outcome) "chat-terminal-request")
                  (string= (provider-error-response-id outcome)
                           "chat-terminal-response")
                  (not (typep outcome 'provider-retryable-error)))
             "Chat truncation retains sanitized terminal metadata"))
          (when (typep outcome 'response-stream-error)
            (test-assert (typep outcome 'provider-retryable-error)
             "unterminated Chat streams remain retryable"))
          (when (string= name "permanent structured error")
            (test-assert (not (typep outcome 'provider-retryable-error))
             "permanent Chat stream errors do not retry"))))))
  nil)

(defun run-chat-tests ()
  "Run Chat Completions encoding and streamed terminal fixtures."
  (let ((provider (make-instance 'chat-completions-provider)))
    (let* ((call-a
            (json-object "type" "function_call" "call_id" "call-a" "namespace" "fs"
             "name" "read" "arguments" "{}"))
           (call-b
            (json-object "type" "function_call" "call_id" "call-b" "namespace" "fs"
             "name" "write" "arguments" "{}"))
           (tool-output
            (json-object "type" "function_call_output" "call_id" "call-a" "output"
             "done"))
           (reasoning-item
            (json-object "type" "reasoning_content" "content" "let me think"))
           (messages
            (openai-compatible--chat-input-messages
             (list reasoning-item call-a call-b tool-output))))
      (test-assert
       (and (= (length messages) 2)
            (string= (json-get (first messages) "role") "assistant")
            (= (length (json-get (first messages) "tool_calls")) 2))
       "multiple function calls share one Chat Completions assistant message")
      (test-assert
       (string= (json-get (first messages) "reasoning_content") "let me think")
       "captured thinking rides on the same round's tool-call message")
      (test-assert (clinker-transcript:family-private-item-p reasoning-item)
       "thinking items stay private to their producing family")
      (test-assert (string= (json-get (second messages) "role") "tool")
       "Chat Completions tool results follow the grouped assistant message"))
    (openai-compatible-provider-tests--terminal-semantics provider)
    (let* ((text-event
            (openai-compatible-provider-tests--stream-event
             (json-object "content" "hello") "chat-test"))
           (reasoning-event
            (openai-compatible-provider-tests--stream-event
             (json-object "reasoning_content" "pondering") "chat-test"))
           (first-tool
            (json-object "index" 0 "id" "call-test" "function"
             (json-object "name" (openai-compatible--wire-tool-name "fs" "read")
              "arguments" "{\"path\":")))
           (second-tool
            (json-object "index" 0 "function" (json-object "arguments" "\"x\"}")))
           (first-tool-event
            (openai-compatible-provider-tests--stream-event
             (json-object "tool_calls" (json-array first-tool)) "chat-test"))
           (second-tool-event
            (openai-compatible-provider-tests--stream-event
             (json-object "tool_calls" (json-array second-tool)) "chat-test"))
           (usage
            (json-object "prompt_tokens" 7 "completion_tokens" 3 "total_tokens" 10))
           (finish-event
            (openai-compatible-provider-tests--stream-event (json-object) "chat-test"
             "tool_calls"))
           (usage-event
            (json-object "id" "chat-test" "choices" (json-array) "usage" usage))
           (source
            (concatenate 'string (format nil "data:~%~%")
                         (test-sse-event-string reasoning-event)
                         (test-sse-event-string text-event)
                         (test-sse-event-string first-tool-event)
                         (test-sse-event-string second-tool-event)
                         (test-sse-event-string finish-event)
                         (test-sse-event-string usage-event)
                         (format nil "data: [DONE]~%~%")))
           (events nil)
           (result
            (provider-consume-stream provider (make-string-input-stream source) nil
                                     (lambda (event) (push event events))))
           (call (first (provider-result-tool-calls result))))
      (test-assert
       (and (string= (provider-result-response-id result) "chat-test")
            (eq (provider-result-turn-completion result) ':continue)
            (= (length (provider-result-output-items result)) 3)
            (= (json-get (provider-result-usage result) "prompt_tokens") 7)
            (= (json-get (provider-result-usage result) "completion_tokens") 3))
       "Chat Completions streams produce usage-aware continuing results")
      (test-assert
       (let ((item (first (provider-result-output-items result))))
         (and (clinker-transcript:chat-reasoning-item-p item)
              (string= (json-get item "content") "pondering")))
       "streamed thinking persists as the leading reasoning item")
      (test-assert
       (and call (string= (json-get call "namespace") "fs")
            (string= (json-get call "name") "read")
            (string= (json-get call "arguments") "{\"path\":\"x\"}"))
       "fragmented Chat Completions tool calls become namespaced calls")
      (test-assert
       (and (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
            (find-if (lambda (event) (typep event 'provider-completed-event)) events))
       "Chat Completions streams emit assistant and completion events")
      (test-assert
       (handler-case
        (progn
         (provider-consume-stream provider
                                  (make-string-input-stream
                                   (test-sse-event-string text-event))
                                  nil #'identity)
         nil)
        (response-stream-error nil t))
       "truncated Chat Completions streams remain retryable failures"))
    (test-assert
     (handler-case
      (progn
       (provider-consume-stream provider
                                (make-string-input-stream (format nil "data: {~%~%"))
                                nil #'identity)
       nil)
      (provider-protocol-error (condition)
       (not (typep condition 'provider-retryable-error))))
     "malformed Chat Completions events are terminal protocol failures")
    (let* ((tool-event
            (openai-compatible-provider-tests--stream-event
             (json-object "tool_calls"
              (json-array
               (json-object "index" 0 "id" "call-empty" "function"
                (json-object "name" (openai-compatible--wire-tool-name "fs" "read")))))
             "chat-empty"))
           (finish-event
            (openai-compatible-provider-tests--stream-event (json-object) "chat-empty"
             "tool_calls"))
           (result
            (provider-consume-stream provider
                                     (make-string-input-stream
                                      (concatenate 'string
                                                   (test-sse-event-string tool-event)
                                                   (test-sse-event-string finish-event)
                                                   (format nil "data: [DONE]~%~%")))
                                     nil #'identity))
           (call (first (provider-result-tool-calls result))))
      (test-assert (and call (string= (json-get call "arguments") "{}"))
       "empty Chat Completions tool arguments normalize to an object"))
    (dolist
        (case
         (list
          (list "missing call id"
                (json-object "index" 0 "function"
                 (json-object "name" "read" "arguments" "{}")))
          (list "missing function name"
                (json-object "index" 0 "id" "call-missing-name" "function"
                 (json-object "arguments" "{}")))))
      (let* ((tool-event
              (openai-compatible-provider-tests--stream-event
               (json-object "tool_calls" (json-array (second case)))
               "chat-incomplete"))
             (finish-event
              (openai-compatible-provider-tests--stream-event (json-object)
               "chat-incomplete" "tool_calls"))
             (source
              (concatenate 'string (test-sse-event-string tool-event)
                           (test-sse-event-string finish-event)
                           (format nil "data: [DONE]~%~%"))))
        (test-assert
         (handler-case
          (progn
           (provider-consume-stream provider (make-string-input-stream source) nil
                                    #'identity)
           nil)
          (provider-protocol-error (condition)
           (not (typep condition 'provider-retryable-error))))
         (format nil "Chat Completions rejects ~A as a terminal protocol failure"
                 (first case)))))
    (dolist
        (item
         (list (json-object "type" 17 "role" "assistant")
               (json-object "type" "message" "role" 17)
               (json-object "type" 17 "summary" (json-array))))
      (test-assert
       (and (null (response-item-assistant-text item))
            (null (response-item-reasoning-summary item))
            (not (clinker-transcript:function-call-item-p item)))
       "non-string response discriminators are ignored safely"))
    (let* ((malformed-event
            (openai-compatible-provider-tests--stream-event
             (json-object "content" 17 "tool_calls"
              (json-array
               (json-object "index" 0 "id" 17 "function"
                (json-object "name" 17 "arguments" 17))))
             "chat-malformed"))
           (finish-event
            (openai-compatible-provider-tests--stream-event (json-object)
             "chat-malformed" "stop"))
           (source
            (concatenate 'string (test-sse-event-string malformed-event)
                         (test-sse-event-string finish-event)
                         (format nil "data: [DONE]~%~%"))))
      (test-assert
       (handler-case
        (progn
         (provider-consume-stream provider (make-string-input-stream source) nil
                                  #'identity)
         nil)
        (provider-protocol-error (condition)
         (not (typep condition 'provider-retryable-error))))
       "malformed tool fields become terminal protocol failures"))))
