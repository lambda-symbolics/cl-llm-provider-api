(in-package #:cl-llm-provider-api)

;;;; -- Anthropic Wire Tests --

(defun anthropic-provider-test--sse-event (event)
  "Return EVENT as one SSE data payload."
  (format nil "data: ~A~2%" (json-encode event)))

(defun anthropic-provider-test--stream-source (events)
  "Return EVENTS encoded as one Anthropic SSE stream."
  (with-output-to-string (stream)
    (dolist (event events)
      (write-string (anthropic-provider-test--sse-event event) stream))))

(defun anthropic-provider-test--message-start
    (&key (id "msg-test")
          (usage (json-object "input_tokens" 42
                              "output_tokens" 1
                              "cache_read_input_tokens" 30
                              "cache_creation_input_tokens" 20)))
  "Return one valid Anthropic message_start event."
  (json-object
   "type" "message_start"
   "message"
   (json-object "id" id
                "type" "message"
                "role" "assistant"
                "model" "claude-haiku-4-5-20251001"
                "content" (json-array)
                "stop_reason" nil
                "stop_sequence" nil
                "usage" usage)))

(defun anthropic-provider-test--message-delta
    (stop-reason &key (usage (json-object "output_tokens" 7)))
  "Return one Anthropic message_delta with STOP-REASON and USAGE."
  (json-object "type" "message_delta"
               "delta" (json-object "stop_reason" stop-reason)
               "usage" usage))

(defun anthropic-provider-test--consume
    (provider events &key headers (event-callback #'identity))
  "Consume Anthropic EVENTS through PROVIDER."
  (provider-consume-stream
   provider
   (make-string-input-stream
    (anthropic-provider-test--stream-source events))
   headers
   event-callback))

(defun anthropic-provider-test--consume-condition (provider events &key headers)
  "Return the provider condition signaled while consuming EVENTS, or NIL."
  (handler-case
      (progn
        (anthropic-provider-test--consume provider events :headers headers)
        nil)
    (provider-error (condition)
      condition)))

(defun anthropic-provider-test--stream-decoding ()
  "Test Anthropic SSE decoding into a normalized provider result."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (events nil)
         (result
           (anthropic-provider-test--consume
            provider
            (list
             (anthropic-provider-test--message-start)
             (json-object "type" "ping")
             (json-object "type" "future_metadata" "value" 1)
             (json-object "type" "content_block_start"
                          "index" 0
                          "content_block"
                          (json-object "type" "text" "text" ""))
             (json-object "type" "content_block_delta"
                          "index" 0
                          "delta" (json-object "type" "text_delta"
                                               "text" "hel"))
             (json-object "type" "content_block_delta"
                          "index" 0
                          "delta" (json-object "type" "text_delta"
                                               "text" "lo"))
             (json-object "type" "content_block_stop" "index" 0)
             (json-object "type" "content_block_start"
                          "index" 1
                          "content_block"
                          (json-object
                           "type" "tool_use"
                           "id" "toolu-1"
                           "name"
                           (openai-compatible--wire-tool-name "fs" "read")
                           "input" (json-object)))
             (json-object "type" "content_block_delta"
                          "index" 1
                          "delta" (json-object "type" "input_json_delta"
                                               "partial_json" "{\"path\":"))
             (json-object "type" "content_block_delta"
                          "index" 1
                          "delta" (json-object "type" "input_json_delta"
                                               "partial_json" "\"x\"}"))
             (json-object "type" "content_block_stop" "index" 1)
             (anthropic-provider-test--message-delta "tool_use")
             (json-object "type" "message_stop"))
            :event-callback (lambda (event) (push event events))))
         (usage (provider-result-usage result))
         (call (first (provider-result-tool-calls result))))
    (test-assert (string= (provider-result-response-id result) "msg-test")
                 "Anthropic streams retain the message identifier")
    (test-assert (= (length (provider-result-output-items result)) 2)
                 "completed content blocks become ordered output items")
    (test-assert
     (string= (or (provider-result-assistant-text result) "") "hello")
     "fragmented text deltas assemble the assistant message")
    (test-assert
     (and call
          (string= (json-get call "namespace") "fs")
          (string= (json-get call "name") "read")
          (string= (json-get call "call_id") "toolu-1")
          (string= (json-get call "arguments") "{\"path\":\"x\"}"))
     "fragmented tool_use input becomes a namespaced function call")
    (test-assert
     (and (= (json-get usage "input_tokens") 92)
          (= (json-get usage "uncached_input_tokens") 42)
          (= (json-get usage "cache_read_input_tokens") 30)
          (= (json-get usage "cache_creation_input_tokens") 20)
          (= (json-get usage "output_tokens") 7)
          (= (json-get usage "total_tokens") 99))
     "Anthropic usage preserves cache reads, writes, and total input")
    (test-assert
     (and (eq (provider-result-turn-completion result) ':continue)
          (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
          (find-if (lambda (event) (typep event 'provider-completed-event))
                   events))
     "tool_use streams emit deltas and request a provider follow-up")
    (test-assert
     (handler-case
         (progn
           (anthropic-provider-test--consume
            provider
            (list (anthropic-provider-test--message-start)))
           nil)
       (response-stream-error ()
         t))
     "streams closing before message_stop remain retryable failures")
    (test-assert
     (handler-case
         (progn
           (anthropic-provider-test--consume
            provider
            (list
             (json-object "type" "error"
                          "error"
                          (json-object "type" "invalid_request_error"
                                       "message" "broken"))))
           nil)
       (provider-error ()
         t))
     "stream error events surface as provider failures"))
  nil)

(defun anthropic-provider-test--stop-reasons ()
  "Test Anthropic turn, tool, incomplete, and invalid stop semantics."
  (let* ((provider (make-instance 'anthropic-messages-provider)))
    (let ((result
            (anthropic-provider-test--consume
             provider
             (list (anthropic-provider-test--message-start)
                   (anthropic-provider-test--message-delta "end_turn")
                   (json-object "type" "message_stop")))))
      (test-assert (eq (provider-result-turn-completion result) ':end)
                   "end_turn completes an Anthropic turn"))
    (let* ((wire-name (openai-compatible--wire-tool-name "fs" "read"))
           (result
             (anthropic-provider-test--consume
              provider
              (list
               (anthropic-provider-test--message-start)
               (json-object "type" "content_block_start" "index" 0
                            "content_block"
                            (json-object "type" "tool_use"
                                         "id" "tool-initial"
                                         "name" wire-name
                                         "input" (json-object "path" "x")))
               (json-object "type" "content_block_stop" "index" 0)
               (anthropic-provider-test--message-delta "tool_use")
               (json-object "type" "message_stop")))))
      (test-assert
       (and (eq (provider-result-turn-completion result) ':continue)
            (first (provider-result-tool-calls result)))
       "tool_use continues the Anthropic turn with a normalized call"))
    (let ((condition
            (anthropic-provider-test--consume-condition
             provider
             (list (anthropic-provider-test--message-start)
                   (anthropic-provider-test--message-delta "max_tokens")
                   (json-object "type" "message_stop"))
             :headers '(("request-id" . "request-incomplete")))))
      (test-assert
       (and (typep condition 'provider-incomplete-response)
            (string= (provider-incomplete-response-reason condition)
                     "max_tokens")
            (string= (provider-error-request-id condition)
                     "request-incomplete"))
       "max_tokens preserves Anthropic incomplete-response semantics"))
    (let ((condition
            (anthropic-provider-test--consume-condition
             provider
             (list (anthropic-provider-test--message-start)
                   (anthropic-provider-test--message-delta "future_reason")
                   (json-object "type" "message_stop")))))
      (test-assert
       (and (typep condition 'provider-error)
            (not (typep condition 'provider-retryable-error))
            (string= (provider-error-code condition) "invalid_stream"))
       "unknown Anthropic stop reasons fail explicitly"))
    (let ((condition
            (anthropic-provider-test--consume-condition
             provider
             (list (anthropic-provider-test--message-start)
                   (anthropic-provider-test--message-delta "pause_turn")
                   (json-object "type" "message_stop")))))
      (test-assert
       (and (typep condition 'provider-error)
            (not (typep condition 'provider-retryable-error))
            (search "pause_turn" (provider-api-error-message condition)))
       "pause_turn is rejected instead of starting a blind follow-up"))
    (let* ((credential "synthetic-credential-in-stream")
           (*provider-active-credential-values* (list credential))
           (*provider-active-credential-redaction-marker* "[REDACTED]")
           (condition
             (anthropic-provider-test--consume-condition
              provider
              (list (anthropic-provider-test--message-start)
                    (anthropic-provider-test--message-delta credential)
                    (json-object "type" "message_stop")))))
      (test-assert
       (and (not (search credential (provider-api-error-message condition)))
            (not (search credential (or (provider-error-response condition) "")))
            (search "[REDACTED]" (or (provider-error-response condition) "")))
       "malformed Anthropic stream errors redact active credentials")))
  nil)

(defun anthropic-provider-test--stream-ordering ()
  "Test that live and completed Anthropic blocks retain wire order."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (events nil)
         (result
           (anthropic-provider-test--consume
            provider
            (list
             (anthropic-provider-test--message-start)
             (json-object "type" "content_block_start" "index" 0
                          "content_block"
                          (json-object "type" "text" "text" "first"))
             (json-object "type" "content_block_stop" "index" 0)
             (json-object "type" "content_block_start" "index" 1
                          "content_block"
                          (json-object "type" "text" "text" "second"))
             (json-object "type" "content_block_stop" "index" 1)
             (anthropic-provider-test--message-delta "end_turn")
             (json-object "type" "message_stop"))
            :event-callback (lambda (event) (push event events))))
         (items (provider-result-output-items result))
         (ordered-events (nreverse events))
         (item-events
           (remove-if-not (lambda (event) (typep event 'provider-item-event))
                          ordered-events))
         (delta-events
           (remove-if-not (lambda (event) (typep event 'assistant-delta-event))
                          ordered-events)))
    (test-assert
     (equal
      (loop for event in delta-events collect (assistant-delta-event-text event))
      '("first" "second"))
     "live assistant deltas follow Anthropic content index order")
    (test-assert
     (equal
      (loop for item in items
            collect (json-get (aref (json-get item "content") 0) "text"))
      '("first" "second"))
     "provider results retain Anthropic content index order")
    (test-assert
     (equal
      (loop for event in item-events
            for item = (provider-item-event-item event)
            collect (json-get (aref (json-get item "content") 0) "text"))
      '("first" "second"))
     "completed item events follow the same content index order"))
  nil)

(defun anthropic-provider-test--stream-lifecycle ()
  "Test representative Anthropic stream lifecycle schema violations."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (start (anthropic-provider-test--message-start))
         (text-start
           (json-object "type" "content_block_start" "index" 0
                        "content_block"
                        (json-object "type" "text" "text" ""))))
    (dolist
        (events
         (list
          (list text-start)
          (list (anthropic-provider-test--message-start
                 :usage (json-object "input_tokens" "42"
                                     "output_tokens" 1)))
          (list start start)
          (list start
                (json-object "type" "content_block_start" "index" 0
                             "content_block"
                             (json-object "type" "thinking"
                                          "thinking" "hidden")))
          (list start text-start
                (anthropic-provider-test--message-delta "end_turn"))))
      (let ((condition
              (anthropic-provider-test--consume-condition
               provider events
               :headers '(("request-id" . "request-malformed")))))
        (test-assert
         (and (typep condition 'provider-error)
              (not (typep condition 'provider-retryable-error))
              (string= (provider-error-code condition) "invalid_stream")
              (string= (provider-error-request-id condition)
                       "request-malformed"))
         "Anthropic lifecycle schema violations signal typed failures"))))
  nil)


(defun anthropic-provider-test--request-condition (provider items)
  "Return the terminal condition while encoding projected ITEMS, or NIL."
  (handler-case
   (progn
    (provider-request-object provider
                             (make-instance 'wire-request :model "claude-haiku-4-5-20251001"
                                            :items items)
                             #())
    nil)
   (provider-error (condition) condition)))


(defun anthropic-provider-test--request-conversion-failures ()
  "Test representative failures for persisted content Anthropic cannot preserve."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (cases
          (list
           (list "unsupported message content"
                 (list
                  (json-object "type" "message" "role" "assistant" "content"
                               (json-array (json-object "type" "future_content" "value" 1)))))
           (list "mismatched tool result"
                 (list
                  (json-object "type" "function_call" "call_id" "expected" "name" "read"
                               "arguments" "{}")
                  (json-object "type" "function_call_output" "call_id" "other" "output"
                               "done")))
           (list "duplicate function-call identifier"
                 (list
                  (json-object "type" "function_call" "call_id" "duplicate" "name" "read"
                               "arguments" "{}")
                  (json-object "type" "function_call" "call_id" "duplicate" "name" "read"
                               "arguments" "{}"))))))
    (dolist (case cases)
      (let ((condition (anthropic-provider-test--request-condition provider (second case))))
        (test-assert
         (and (typep condition 'provider-error)
              (string= (provider-error-code condition) "unsupported_content"))
         (format nil "Anthropic rejects persisted ~A" (first case)))))
    (let ((fallback
           (anthropic--tool-result-block
            (json-object "type" "function_call_output" "call_id" "fallback" "output"
                         (json-array (json-object "type" "future_content" "value" 1))))))
      (test-assert (stringp (json-get fallback "content"))
       "an all-unsupported tool result uses a bounded textual fallback")))
  nil)


(defun anthropic-provider-test--request-encoding ()
  "Test Messages request shape: system blocks, alternation, and tool encoding."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (items
          (list
           (json-object "type" "message" "role" "user" "content"
                        (json-array
                         (json-object "type" "input_text" "text" "first question")))
           (json-object "type" "message" "role" "assistant" "content"
                        (json-array (json-object "type" "output_text" "text" "first answer")))
           (json-object "type" "function_call" "namespace" "plan" "name" "update" "call_id"
                        "call-1" "arguments" "{\"steps\":[]}")
           (json-object "type" "function_call_output" "call_id" "call-1" "output" "done")
           (json-object "type" "message" "role" "user" "content"
                        (json-array
                         (json-object "type" "input_text" "text" "second question"))))))
    (multiple-value-bind (request delivery)
        (provider-request-object provider
                                 (make-instance 'wire-request :model
                                                "claude-haiku-4-5-20251001" :items items
                                                :prefix '("Stable instructions") :suffix
                                                '("remember the goal" "Temporary context"))
                                 (provider-wire-tools provider
                                                      (json-array
                                                       (json-object "type" "namespace" "name"
                                                                    "plan" "tools"
                                                                    (json-array
                                                                     (json-object "name"
                                                                                  "update"
                                                                                  "description"
                                                                                  "Update the plan."
                                                                                  "parameters"
                                                                                  (json-object
                                                                                   "type"
                                                                                   "object")))))))
      (declare (ignore delivery))
      (let ((messages (json-get request "messages"))
            (system (json-get request "system"))
            (tools (json-get request "tools")))
        (test-assert (string= (json-get request "model") "claude-haiku-4-5-20251001")
         "the request selects the configured Claude model")
        (test-assert
         (and (integerp (json-get request "max_tokens")) (eq (json-get request "stream") t))
         "the request streams with a bounded output budget")
        (test-assert
         (and (vectorp system) (= (length system) 1)
              (json-string= (json-get (json-get (aref system 0) "cache_control") "type")
                            "ephemeral")
              (not (search "remember the goal" (json-get (aref system 0) "text"))))
         "the stable system prompt ends at an explicit cache breakpoint")
        (test-assert
         (equal
          (loop for message across messages
                collect (json-get message "role"))
          '("user" "assistant" "user"))
         "calls, results, and text merge into strict user/assistant alternation")
        (let ((assistant
               (find "assistant" (coerce messages 'list) :key
                     (lambda (message) (json-get message "role")) :test #'string=)))
          (test-assert
           (equal
            (loop for block across (json-get assistant "content")
                  collect (json-get block "type"))
            '("text" "tool_use"))
           "assistant messages join text and tool_use blocks in order"))
        (let* ((final-user (first (last (coerce messages 'list))))
               (content (json-get final-user "content")))
          (test-assert
           (equal
            (loop for block across content
                  collect (json-get block "type"))
            '("tool_result" "text" "text" "text"))
           "goal and mutable context append after durable user history")
          (test-assert
           (and
            (json-string= (json-get (json-get (aref content 1) "cache_control") "type")
                          "ephemeral")
            (string= (json-get (aref content 2) "text") "remember the goal")
            (null (json-get (aref content 2) "cache_control"))
            (search "Temporary context" (json-get (aref content 3) "text"))
            (null (json-get (aref content 3) "cache_control")))
           "history is cache-marked immediately before volatile context"))
        (let ((assistant-calls
               (loop for message across messages
                     when (string= (json-get message "role") "assistant")
                     append (loop for block across (json-get message "content")
                                  when (json-string= (json-get block "type") "tool_use")
                                  collect block))))
          (test-assert (= (length assistant-calls) 1) "function calls become tool_use blocks")
          (test-assert
           (and (string= (json-get (first assistant-calls) "id") "call-1")
                (json-object-p (json-get (first assistant-calls) "input")))
           "tool_use blocks carry the call id and decoded input"))
        (let ((results
               (loop for message across messages
                     when (string= (json-get message "role") "user")
                     append (loop for block across (json-get message "content")
                                  when (json-string= (json-get block "type") "tool_result")
                                  collect block))))
          (test-assert
           (and (= (length results) 1)
                (string= (json-get (first results) "tool_use_id") "call-1")
                (string= (json-get (first results) "content") "done"))
           "function outputs become user tool_result blocks"))
        (test-assert
         (and (= (length tools) 1) (json-object-p (json-get (aref tools 0) "input_schema"))
              (json-string= (json-get (json-get (aref tools 0) "cache_control") "type")
                            "ephemeral")
              (json-object-p (json-get request "tool_choice")))
         "the final flattened tool is an explicit cache breakpoint")
        (let ((wire-name (json-get (aref tools 0) "name")))
          (multiple-value-bind (namespace name)
              (openai-compatible--decode-wire-tool-name wire-name)
            (test-assert (and (string= namespace "plan") (string= name "update"))
             "wire tool names round-trip through the readable encoding"))))))
  nil)


(defun anthropic-provider-test--portable-content ()
  "Test cross-provider refusal and multimodal tool-result encoding."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (data-url "data:image/png;base64,AA==")
         (items
          (list
           (json-object #1="type" #2="message" #3="role" #4="user" #5="content"
                        (json-array
                         (json-object #6="type" #7="input_text" #8="text" "Show the image.")))
           (json-object "type" "function_call" "namespace" "fs" "name" "view-image" "call_id"
                        "view-1" "arguments" "{}")
           (json-object "type" "function_call_output" "call_id" "view-1" "output"
                        (json-array (json-object "type" "input_text" "text" "Before image.")
                                    (json-object "type" "input_image" "image_url" data-url)
                                    (json-object "type" "input_text" "text" "After image.")
                                    (json-object "type" "input_image" "image_url"
                                                 "https://example.com/image.png")))
           (json-object "type" "message" "role" "assistant" "content"
                        (json-array
                         (json-object "type" "refusal" "text" "I cannot do that.")))
           (json-object #1# #2# #3# #4# #5#
                        (json-array (json-object #6# #7# #8# "Try this instead."))))))
    (multiple-value-bind (request delivery)
        (provider-request-object provider
                                 (make-instance 'wire-request :model
                                                "claude-haiku-4-5-20251001" :items items)
                                 #())
      (declare (ignore delivery))
      (let* ((messages (json-get request "messages"))
             (tool-result
              (loop for message across messages
                    thereis (loop for block across (json-get message "content")
                                  when (json-string= (json-get block "type")
                                                     "tool_result") return block)))
             (content (and tool-result (json-get tool-result "content")))
             (refusal-text
              (loop for message across messages
                    thereis (and (json-string= (json-get message "role") "assistant")
                                 (loop for block across (json-get message "content")
                                       thereis (and
                                                (json-string= (json-get block "type") "text")
                                                (string= (json-get block "text")
                                                         "I cannot do that.")
                                                (json-get block "text")))))))
        (test-assert
         (equal
          (loop for message across messages
                collect (json-get message "role"))
          '("user" "assistant" "user" "assistant" "user"))
         "durable refusal replay preserves surrounding message roles")
        (test-assert
         (and (vectorp content)
              (equal
               (loop for block across content
                     collect (json-get block "type"))
               '("text" "image" "text" "image"))
              (string= (json-get (aref content 0) "text") "Before image.")
              (string= (json-get (json-get (aref content 1) "source") "data") "AA==")
              (string= (json-get (aref content 2) "text") "After image.")
              (string= (json-get (json-get (aref content 3) "source") "url")
                       "https://example.com/image.png"))
         "durable tool results preserve base64, URL image, and text order")
        (test-assert (string= refusal-text "I cannot do that.")
         "durable refusal content becomes Anthropic assistant text"))))
  nil)


(defun anthropic-provider-test--cache-boundaries ()
  "Test explicit cache boundaries using only projected request values."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (items
           (list (json-object "type" "message" "role" "user" "content" "question")
                 (json-object "type" "message" "role" "assistant" "content" "answer")))
         (ephemeral
           (list (json-object "type" "function_call" "namespace" "tools"
                              "name" "read" "call_id" "volatile" "arguments" "{}")))
         (tools
           (provider-wire-tools
            provider
            (json-array
             (json-object "type" "namespace" "name" "tools"
                          "tools" (json-array
                                   (json-object "name" "read" "description" "Read"
                                                "parameters" (json-object)))))))
         (projection
           (make-instance 'wire-request :model "claude-test" :items items
                          :prefix '("prefix") :suffix '("suffix")
                          :options (list :ephemeral-items ephemeral
                                         :maximum-output-tokens 12)))
         (request (provider-request-object provider projection tools))
         (messages (json-get request "messages"))
         (assistant-content (json-get (aref messages 1) "content")))
    (test-assert
     (and (= (json-get request "max_tokens") 12)
          (= (length assistant-content) 2)
          (json-object-p (json-get (aref assistant-content 0) "cache_control"))
          (json-string= (json-get (aref assistant-content 1) "type") "tool_use")
          (null (json-get (aref assistant-content 1) "cache_control")))
     "volatile calls follow the durable cache boundary")
    (test-assert
     (and (not (search "cache_control" (json-encode items)))
          (not (search "cache_control" (json-encode ephemeral)))
          (not (search "cache_control" (json-encode tools))))
     "cache annotations do not mutate caller-owned history or reusable tools")
    (test-assert
     (and (= (length messages) 3)
          (json-string= (json-get (aref messages 2) "role") "user")
          (null (json-get (aref (json-get (aref messages 2) "content") 0)
                         "cache_control")))
     "request-local suffix text follows the cache-marked history")
    (test-assert
     (not (search
           "cache_control"
           (json-encode
            (provider-request-object
             provider
             (make-instance 'wire-request :model "claude-test" :items items
                            :prefix '("prefix") :options '(:cache-p nil))
             tools))))
     "one-off requests disable cache annotations on history, system, and tools")
    (let* ((guidance
             (list (json-object "type" "message" "role" "developer" "content" "d")
                   (json-object "type" "message" "role" "system" "content" "s")))
           (request
             (provider-request-object
              provider
              (make-instance 'wire-request :model "claude-test"
                             :items (append items guidance)
                             :prefix '("prefix"))
              #()))
           (content (json-get (aref (json-get request "messages") 2) "content")))
      (test-assert
       (and (equal (loop for block across content collect (json-get block "text"))
                   '("d" "s"))
            (= (length (json-get request "system")) 1))
       "positional developer and system items are not hoisted into the prefix")))
  nil)

(defun anthropic-provider-test--execution ()
  "Exercise injected transport, deadlines, cleanup, and secret-safe semantic events."
  (let* ((provider (make-instance 'anthropic-messages-provider))
         (secret "synthetic-anthropic-secret")
         (request (json-object "model" "claude-test"))
         (source
           (anthropic-provider-test--stream-source
            (list (anthropic-provider-test--message-start :id secret)
                  (json-object "type" "content_block_start" "index" 0
                               "content_block" (json-object "type" "text" "text" secret))
                  (json-object "type" "content_block_stop" "index" 0)
                  (anthropic-provider-test--message-delta "end_turn")
                  (json-object "type" "message_stop"))))
         (stream (make-string-input-stream source))
         (cleaned-p nil)
         (deadline-calls 0)
         (completed-p nil)
         (events nil)
         (result
           (provider-execute-request
            provider request :secrets (list secret)
            :transport (lambda (projected)
                         (test-assert (eq projected request)
                                      "transport receives the projected request")
                         (values stream 200 (list (cons "request-id" secret))))
            :call-with-deadline (lambda (function)
                                  (incf deadline-calls)
                                  (funcall function))
            :cleanup (lambda (input)
                       (close input)
                       (setf cleaned-p t))
            :completion (lambda ()
                          (test-assert cleaned-p "completion follows stream cleanup")
                          (setf completed-p t))
            :event-callback (lambda (event) (push event events)))))
    (test-assert (and cleaned-p completed-p (= deadline-calls 1)
                      (not (open-stream-p stream)))
                 "successful Anthropic execution honors lifecycle hooks")
    (test-assert
     (and (not (search secret (provider-result-response-id result)))
          (not (search secret (provider-result-assistant-text result)))
          (every (lambda (event)
                   (or (not (typep event 'assistant-delta-event))
                       (not (search secret (assistant-delta-event-text event)))))
                 events))
     "redaction precedes semantic events and the final provider result")
    (dolist (case
             (list
              (list "529 overload" 529 "{}" 'provider-retryable-error)
              (list "truncated stream" 200
                    (anthropic-provider-test--stream-source
                     (list (anthropic-provider-test--message-start)
                           (json-object "type" "content_block_start" "index" 0
                                        "content_block"
                                        (json-object "type" "text" "text" "partial"))
                           (json-object "type" "content_block_stop" "index" 0)))
                    'response-stream-error)
              (list "incomplete response" 200
                    (anthropic-provider-test--stream-source
                     (list (anthropic-provider-test--message-start)
                           (anthropic-provider-test--message-delta "max_tokens")))
                    'provider-incomplete-response)
              (list "invalid lifecycle" 200
                    (anthropic-provider-test--stream-source
                     (list (json-object "type" "message_stop")))
                    'provider-protocol-error)))
      (let* ((input (make-string-input-stream (third case)))
             (events nil)
             (cleanup-count 0)
             (completed-p nil)
             (condition
               (handler-case
                   (provider-execute-request
                    provider request
                    :transport (lambda (projected)
                                 (declare (ignore projected))
                                 (values input (second case) nil))
                    :cleanup (lambda (input) (incf cleanup-count) (close input))
                    :completion (lambda () (setf completed-p t))
                    :event-callback (lambda (event) (push event events)))
                 (provider-error (condition) condition))))
        (test-assert
         (and (typep condition (fourth case)) (= cleanup-count 1)
              (not (open-stream-p input)) (not completed-p)
              (notany (lambda (event)
                        (or (typep event 'provider-item-event)
                            (typep event 'provider-completed-event)))
                      events))
         (format nil "~A cleans up without publishing completed items" (first case))))))
  nil)

(defun run-anthropic-tests ()
  "Run the standalone Anthropic wire checks and return their count."
  (let ((*wire-test-checks* 0))
    (anthropic-provider-test--request-conversion-failures)
    (anthropic-provider-test--request-encoding)
    (anthropic-provider-test--portable-content)
    (anthropic-provider-test--cache-boundaries)
    (anthropic-provider-test--stream-decoding)
    (anthropic-provider-test--stop-reasons)
    (anthropic-provider-test--stream-ordering)
    (anthropic-provider-test--stream-lifecycle)
    (anthropic-provider-test--execution)
    *wire-test-checks*))
