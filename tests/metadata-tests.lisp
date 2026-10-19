(in-package #:cl-llm-provider-api/tests)

(defclass metadata-test-code-assist-provider (gemini-generate-content-provider) ()
  (:documentation "Exercise metadata after a host's Code Assist envelope adapter."))

(defmethod provider-gemini-stream-response ((provider metadata-test-code-assist-provider) event)
  (declare (ignore provider))
  (let ((response (json-get event "response")))
    (values response (json-get response "responseId"))))

(defclass metadata-test-codex-provider (responses-api-provider) ()
  (:documentation "Stand in for a host adapter with a synthetic cache-write zero."))

(defmethod provider-cache-write-tokens ((provider metadata-test-codex-provider) usage)
  "Let host adapters exclude structural zeros without changing wire semantics."
  (let ((tokens (call-next-method)))
    (unless (eql tokens 0) tokens)))

(defun metadata-test--body (events)
  "Encode event objects and the optional DONE marker as SSE."
  (with-output-to-string (stream)
    (dolist (event events)
      (format stream "data: ~A~%~%" (if (stringp event) event (json-encode event))))))

(defun metadata-test--consume (provider events)
  "Return a result or provider condition and the semantic events received."
  (let ((emitted nil))
    (values
     (handler-case
         (with-input-from-string (stream (metadata-test--body events))
           (provider-consume-stream provider stream nil (lambda (event) (push event emitted))))
       (provider-error (condition) condition))
     (nreverse emitted))))

(defun metadata-test--events (protocol &key (model "returned-model") (cache ':missing)
                                          (outcome ':success))
  "Build valid empty responses, incomplete responses, or interrupted streams."
  (let* ((usage (json-object "input_tokens" 3 "output_tokens" 0))
         (gemini-p (eq protocol ':gemini-generate-content))
         (initial nil)
         (terminal nil))
    (unless (eq cache ':missing)
      (setf (gethash (if gemini-p "cacheWriteInputTokens" "cache_creation_input_tokens") usage)
            cache))
    (case protocol
      (:responses-api
       (setf initial (json-object "type" "response.created" "response"
                                  (json-object "id" "r" "model" model "usage" usage))
             terminal (json-object
                       "type" (if (eq outcome ':incomplete) "response.incomplete" "response.completed")
                       "response" (json-object "id" "r" "model" model "usage" usage
                                               "incomplete_details"
                                               (json-object "reason" "max_output_tokens")))))
      (:chat-completions
       (setf initial (json-object "model" model "usage" usage "choices" #())
             terminal (json-object "choices"
                                   (json-array (json-object "delta" (json-object)
                                                            "finish_reason"
                                                            (if (eq outcome ':incomplete) "length" "stop"))))))
      (:anthropic-messages
       (setf initial (json-object "type" "message_start" "message"
                                  (json-object "id" "r" "role" "assistant" "content" #()
                                               "model" model "usage" usage))
             terminal (json-object "type" "message_delta" "delta"
                                   (json-object "stop_reason"
                                                (if (eq outcome ':incomplete) "max_tokens" "end_turn"))
                                   "usage" (json-object "output_tokens" 0))))
      (:gemini-generate-content
       (setf initial (json-object "modelVersion" model "usageMetadata" usage "candidates" #())
             terminal (json-object "candidates"
                                   (json-array (json-object "finishReason"
                                                            (if (eq outcome ':incomplete) "MAX_TOKENS" "STOP")))))))
    (if (eq outcome ':partial)
        (list initial)
        (append (list initial terminal)
                (case protocol
                  (:chat-completions (list "[DONE]"))
                  (:anthropic-messages (list (json-object "type" "message_stop"))))))))

(defun metadata-test--snapshots (events)
  "Return only the immutable response metadata emitted by a stream."
  (loop for event in events when (typep event 'provider-response-metadata-event)
        collect (provider-response-metadata-event-metadata event)))

(defun test-response-metadata-streams ()
  "Exercise successful, incomplete, and interrupted streams across wire adapters."
  (dolist (class '(responses-api-provider chat-completions-provider anthropic-messages-provider
                  gemini-generate-content-provider metadata-test-code-assist-provider))
    (let* ((provider (make-instance class))
           (protocol (provider-wire-protocol provider)))
      (dolist (cache '(:missing 0 7))
        (dolist (outcome '(:success :incomplete :partial))
          (let ((frames (metadata-test--events protocol :cache cache :outcome outcome)))
            (when (eq class 'metadata-test-code-assist-provider)
              (setf frames (mapcar (lambda (frame) (json-object "response" frame)) frames)))
            (multiple-value-bind (result events) (metadata-test--consume provider frames)
              (let* ((snapshots (metadata-test--snapshots events))
                     (metadata (first (last snapshots)))
                     (completed (find-if (lambda (event) (typep event 'provider-completed-event)) events)))
                (check (equal (provider-response-metadata-model metadata) "returned-model")
                       "~S/~S lost the response model" class outcome)
                (check (eql (provider-response-metadata-cache-write-tokens metadata)
                            (unless (eq cache ':missing) cache))
                       "~S/~S lost cache presence or value ~S" class outcome cache)
                (check (provider-response-metadata-usage-observed-p metadata)
                       "~S/~S lost observed usage" class outcome)
                (check (null (provider-response-metadata-finish-reason (first snapshots)))
                       "an earlier snapshot changed after a terminal observation")
                (if (eq outcome ':success)
                    (progn
                      (check (typep result 'provider-result) "~S did not complete: ~S" class result)
                      (check (and completed
                                  (eq metadata (provider-completed-event-metadata completed))
                                  (eq metadata (provider-result-metadata result)))
                             "~S did not share its terminal snapshot" class))
                    (progn
                      (check (typep result 'provider-error) "~S/~S did not fail" class outcome)
                      (check (null completed) "an unsuccessful stream emitted completion")))
                (check (equal (provider-response-metadata-finish-reason metadata)
                              (unless (eq outcome ':partial)
                                (case protocol
                                  (:responses-api (if (eq outcome ':success) "completed" "max_output_tokens"))
                                  (:chat-completions (if (eq outcome ':success) "stop" "length"))
                                  (:anthropic-messages (if (eq outcome ':success) "end_turn" "max_tokens"))
                                  (:gemini-generate-content (if (eq outcome ':success) "STOP" "MAX_TOKENS")))))
                       "~S/~S lost the terminal reason before validation" class outcome)))))))))

(defun test-response-metadata-cache-counters ()
  "Distinguish omitted and invalid counters, explicit zero, and Codex placeholders."
  (let ((provider (make-instance 'model-provider))
        (codex (make-instance 'metadata-test-codex-provider)))
    (dolist (value '(nil -1 "0" 0 12))
      (dolist (usage (list (json-object "cache_creation_input_tokens" value)
                          (json-object "cache_write_tokens" value)
                          (json-object "cache_write_input_tokens" value)
                          (json-object "cacheWriteInputTokens" value)
                          (json-object "input_tokens_details" (json-object "cache_write_tokens" value))
                          (json-object "prompt_tokens_details" (json-object "cache_write_tokens" value))))
        (check (eql (provider-cache-write-tokens provider usage)
                    (and (typep value '(integer 0)) value))
               "cache counter ~S was not preserved exactly" value)
        (check (eql (provider-cache-write-tokens codex usage)
                    (and (typep value '(integer 1)) value))
               "Codex's placeholder zero was treated as measured usage")))
    (dolist (usage (list (json-object "cache_creation_input_tokens" 0
                                     "cache_write_tokens" 7)
                        (json-object "input_tokens_details" (json-object "cache_write_tokens" 0)
                                     "prompt_tokens_details" (json-object "cache_write_tokens" 7))))
      (dolist (instance (list provider codex))
        (check (eql (provider-cache-write-tokens instance usage) 7)
               "a placeholder zero hid a positive compatibility counter")
        (check (eql (json-get (provider-usage-normalize usage) "cache_creation_input_tokens") 7)
               "normalization lost a positive compatibility counter")))
    (multiple-value-bind (result events)
        (metadata-test--consume codex (metadata-test--events ':responses-api :cache 0))
      (declare (ignore events))
      (check (and (null (provider-response-metadata-cache-write-tokens (provider-result-metadata result)))
                  (provider-response-metadata-usage-observed-p (provider-result-metadata result)))
             "Codex zero leaked through the stream boundary"))))

(defun test-response-metadata-envelope-boundaries ()
  "Ignore generated model fields, retain known metadata on failure, and reset retries."
  (let ((provider (make-instance 'test-responses-provider :configuration '(:model "requested-only"))))
    (multiple-value-bind (result events)
        (metadata-test--consume
         provider (list (json-object "type" "response.output_text.delta" "delta" "{\"model\":\"fake\"}"
                                     "model" "wrong-level" "response" (json-object "model" "wrong-envelope"))
                        (json-object "type" "response.completed" "response" (json-object "id" "r"))))
      (declare (ignore events))
      (let ((metadata (provider-result-metadata result)))
        (check (null (provider-response-metadata-model metadata)) "generated or requested model leaked into metadata")
        (check (not (provider-response-metadata-usage-observed-p metadata)) "absent usage became observed")))
    (multiple-value-bind (result events)
        (metadata-test--consume
         provider (list (json-object "type" "response.created" "response"
                                     (json-object "model" "failed-model"))
                        (json-object "type" "response.failed" "response"
                                     (json-object "error" (json-object "code" "server_error" "message" "failed")))))
      (let ((metadata (first (last (metadata-test--snapshots events)))))
        (check (typep result 'provider-error) "a failed stream completed")
        (check (and (equal (provider-response-metadata-model metadata) "failed-model")
                    (equal (provider-response-metadata-finish-reason metadata) "failed"))
               "metadata was unavailable after a failed terminal event")))
    (let ((attempts 0) (emitted nil))
      (let ((result
              (call-with-streaming-retries
               (lambda (callback)
                 (incf attempts)
                 (with-input-from-string
                     (stream (metadata-test--body
                              (metadata-test--events ':responses-api
                                                     :model (when (= attempts 1) "first-attempt")
                                                     :cache (if (= attempts 1) 9 ':missing)
                                                     :outcome (if (= attempts 1) ':partial ':success))))
                   (provider-consume-stream provider stream nil callback)))
               (lambda (event) (push event emitted))
               :maximum-retries 1 :maximum-streaming-retries 0
               :sleep-function #'retry-tests--no-sleep)))
        (check (= attempts 2) "metadata alone consumed the streaming-output retry budget")
        (check (and (null (provider-response-metadata-model (provider-result-metadata result)))
                    (null (provider-response-metadata-cache-write-tokens (provider-result-metadata result))))
               "metadata leaked from a failed attempt into its retry")
        (check (some (lambda (metadata)
                       (equal (provider-response-metadata-model metadata) "first-attempt"))
                     (metadata-test--snapshots emitted))
               "the failed attempt's metadata was discarded before callbacks")))))

(defun test-response-metadata-model-spelling ()
  "Preserve response model spellings and ignore nested generated model fields."
  (dolist (class '(responses-api-provider chat-completions-provider anthropic-messages-provider
                  gemini-generate-content-provider metadata-test-code-assist-provider))
    (let* ((provider (make-instance class))
           (protocol (provider-wire-protocol provider)))
      (dolist (model (list nil (concatenate 'string "Soukromý/模型-" (make-string 300 :initial-element #\x))))
        (let* ((frames (metadata-test--events protocol :model model))
               (initial (first frames)))
          (case protocol
            (:responses-api
             (push (json-object "type" "response.output_text.delta" "delta" "{\"model\":\"fake\"}"
                                "response" (json-object "model" "wrong-envelope")) frames))
            (:chat-completions
             (setf (gethash "choices" initial)
                   (json-array (json-object "delta" (json-object "content" "{\"model\":\"fake\"}"
                                                                "model" "wrong-envelope")))))
            (:anthropic-messages
             (setf (gethash "model" initial) "wrong-envelope"
                   (gethash "model" (json-get (second frames) "delta")) "wrong-envelope"))
            (:gemini-generate-content
             (setf (gethash "candidates" initial)
                   (json-array (json-object "modelVersion" "wrong-envelope"
                                            "content" (json-object "parts"
                                                                   (json-array (json-object "text" "{\"model\":\"fake\"}"))))))))
          (when (eq class 'metadata-test-code-assist-provider)
            (setf frames (mapcar (lambda (frame) (json-object "response" frame
                                                            "modelVersion" "wrong-envelope")) frames)))
          (multiple-value-bind (result events) (metadata-test--consume provider frames)
            (declare (ignore events))
            (check (equal (provider-response-metadata-model (provider-result-metadata result)) model)
                   "~S changed the response spelling or read a generated model" class)))))))

(defun test-response-metadata-invalid-terminals ()
  "Keep model and terminal metadata when subsequent protocol validation fails."
  (dolist (case
            (list (list (make-instance 'responses-api-provider)
                        (list (json-object "type" "response.completed" "response" "invalid")) "completed")
                  (list (make-instance 'chat-completions-provider)
                        (list (json-object "model" "observed" "usage" (json-object "cache_write_tokens" 6)
                                           "choices" (json-array (json-object "finish_reason" "invalid")))) "invalid")
                  (list (make-instance 'anthropic-messages-provider)
                        (list (json-object "type" "message_delta" "delta"
                                           (json-object "stop_reason" "invalid") "usage" "invalid")) "invalid")
                  (list (make-instance 'gemini-generate-content-provider)
                        (list (json-object "modelVersion" "observed"
                                           "candidates" (json-array (json-object "finishReason" "INVALID")))) "INVALID")))
    (destructuring-bind (provider frames reason) case
      (multiple-value-bind (result events) (metadata-test--consume provider frames)
        (check (typep result 'provider-error) "invalid terminal metadata bypassed protocol validation")
        (check (equal (provider-response-metadata-finish-reason
                       (first (last (metadata-test--snapshots events)))) reason)
               "terminal metadata was lost before protocol validation")))))

(defun run-metadata-tests ()
  "Run response metadata contracts at the semantic stream boundary."
  (test-response-metadata-streams)
  (test-response-metadata-cache-counters)
  (test-response-metadata-envelope-boundaries)
  (test-response-metadata-model-spelling)
  (test-response-metadata-invalid-terminals))
