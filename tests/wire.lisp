(in-package #:cl-llm-provider-api)


(defvar *wire-test-checks* 0)

(defun test-assert (value message)
  "Check a portable wire invariant."
  (incf *wire-test-checks*)
  (unless value (error "~A" message)))

(defun test-sse-event-string (event)
  "Encode EVENT as one complete server-sent event."
  (format nil "data: ~A~%~%" (json-encode event)))

(defclass test-character-input-stream (sb-gray:fundamental-character-input-stream)
          ((source :initarg :source :reader test-character-input-stream-source :type
            string :documentation "The deterministic character source.")
           (position :initform 0 :accessor test-character-input-stream-position :type
                     integer :documentation "The next source character offset."))
          (:documentation
           "A test stream implementing character reads but not line reads."))

(defmethod sb-gray:stream-read-char ((stream test-character-input-stream))
  "Read one character from STREAM, returning the Gray-stream EOF marker at its end."
  (let ((position (test-character-input-stream-position stream))
        (source (test-character-input-stream-source stream)))
    (if (< position (length source))
        (prog1 (char source position)
          (incf (test-character-input-stream-position stream)))
        :eof)))

(defun test-provider-usage-normalization ()
  "Test portable prompt-cache counters across provider usage shapes."
  (labels ((assert-case (usage expected-cached expected-created label)
             "Assert one normalized USAGE case named LABEL."
             (let ((normalized (provider-usage-normalize usage)))
               (test-assert
                (= (json-get normalized "cached_input_tokens") expected-cached)
                (format nil "~A cache reads normalize" label))
               (test-assert
                (= (json-get normalized "cache_creation_input_tokens")
                   expected-created)
                (format nil "~A cache writes normalize" label))
               (test-assert
                (and (= (json-get normalized "input_tokens") 100)
                     (= (json-get normalized "output_tokens") 25)
                     (= (json-get normalized "total_tokens") 125))
                (format nil "~A ordinary usage normalizes" label)))))
    (assert-case
     (json-object "input_tokens" 100 "output_tokens" 25 "input_tokens_details"
      (json-object "cached_tokens" 70 "cache_write_tokens" 20))
     70 20 "Responses")
    (assert-case
     (json-object "prompt_tokens" 100 "completion_tokens" 25 "prompt_tokens_details"
      (json-object "cached_tokens" 60 "cache_write_tokens" 10))
     60 10 "Chat Completions")
    (assert-case
     (json-object "prompt_tokens" 100 "completion_tokens" 25 "prompt_cache_hit_tokens"
      55 "cache_creation_input_tokens" 5)
     55 5 "DeepSeek")
    (assert-case
     (json-object "input_tokens" 100 "output_tokens" 25 "cache_read_input_tokens" 50
      "cache_creation_input_tokens" 30)
     50 30 "Anthropic"))
  (multiple-value-bind (value present-p)
      (gethash "cached_input_tokens"
               (provider-usage-normalize (json-object "input_tokens" 100)))
    (declare (ignore value))
    (test-assert (not present-p) "usage without cache counters stays distinguishable"))
  (test-assert (null (provider-usage-normalize nil)) "absent usage stays absent")
  nil)

(defun test-provider-stream-decoding ()
  "Test semantic stream decoding from a deterministic SSE fixture."
  (let* ((message-item
          (json-object "id" "ephemeral-item-id" "type" "message" "role" "assistant"
           "content" (json-array (json-object "type" "output_text" "text" "hello"))))
         (reasoning-item
          (json-object "id" "ephemeral-reasoning-id" "type" "reasoning" "summary"
           (json-array
            (json-object "type" "summary_text" "text" "I inspected the request.")
            (json-object "type" "summary_text" "text" "I chose a safe response."))
           "content"
           (json-array
            (json-object "type" "reasoning_text" "text" "raw private reasoning"))
           "encrypted_content" "opaque-test-ciphertext"))
         (source
          (concatenate 'string
                       (test-sse-event-string
                        (json-object "type" "response.created" "response"
                         (json-object "id" "response-1")))
                       (test-sse-event-string
                        (json-object "type" "response.reasoning_summary_text.delta"
                         "item_id" "ephemeral-reasoning-id" "output_index" 0
                         "summary_index" 0 "delta" "I inspected "))
                       (test-sse-event-string
                        (json-object "type" "response.reasoning_summary_text.delta"
                         "item_id" "ephemeral-reasoning-id" "output_index" 0
                         "summary_index" 0 "delta" "the request."))
                       (test-sse-event-string
                        (json-object "type" "response.reasoning_summary_text.delta"
                         "item_id" "ephemeral-reasoning-id" "output_index" 0
                         "summary_index" 1 "delta" "I chose a safe response."))
                       (test-sse-event-string
                        (json-object "type" "response.reasoning_text.delta" "delta"
                         "raw private reasoning"))
                       (test-sse-event-string
                        (json-object "type" "response.function_call_arguments.delta"
                         "item_id" "call-progress" "delta" "{\"path\":"))
                       (test-sse-event-string
                        (json-object "type" "response.output_text.delta" "delta"
                         "hello"))
                       (test-sse-event-string
                        (json-object "type" "response.output_item.done" "item"
                         message-item))
                       (test-sse-event-string
                        (json-object "type" "response.output_item.done" "item"
                         reasoning-item))
                       (test-sse-event-string
                        (json-object "type" "response.completed" "response"
                         (json-object "id" "response-1" "end_turn" yason:false "usage"
                          (json-object "input_tokens" 5))))))
         (events nil)
         (result
          (provider-consume-stream (make-instance 'model-provider)
                                   (make-instance 'test-character-input-stream :source
                                                  source)
                                   '(("x-codex-turn-state" . "turn-state-1"))
                                   (lambda (event) (push event events)))))
    (test-assert (= (length (provider-result-output-items result)) 2)
     "the stream retains authoritative completed items in wire order")
    (test-assert (string= (provider-result-response-id result) "response-1")
     "the stream retains its response identifier")
    (test-assert (string= (provider-result-turn-state result) "turn-state-1")
     "the stream retains request-local turn state")
    (test-assert (eq (provider-result-turn-completion result) :continue)
     "the stream retains an explicit provider continuation")
    (test-assert (not (gethash "id" (first (provider-result-output-items result))))
     "completed response items discard transient server identifiers")
    (test-assert
     (string=
      (json-get (second (provider-result-output-items result)) "encrypted_content")
      "opaque-test-ciphertext")
     "completed encrypted reasoning remains available for replay")
    (let* ((reasoning-output (second (provider-result-output-items result)))
           (summary (response-item-reasoning-summary reasoning-output)))
      (test-assert
       (string= summary
                (format nil "I inspected the request.~2%I chose a safe response."))
       "completed reasoning exposes only its dedicated visible summary")
      (test-assert (not (search "raw private reasoning" summary))
       "raw reasoning content is never folded into the summary"))
    (let* ((reasoning-events
            (reverse
             (remove-if-not (lambda (event) (typep event 'reasoning-delta-event))
                            events)))
           (streamed-summary
            (format nil "~{~A~}"
                    (mapcar #'reasoning-delta-event-text reasoning-events))))
      (test-assert (= (length reasoning-events) 3)
       "only summary deltas become visible reasoning events")
      (test-assert
       (string= streamed-summary
                (format nil "I inspected the request.~2%I chose a safe response."))
       "summary part boundaries match the authoritative completed text"))
    (test-assert
     (= (count-if (lambda (event) (typep event 'provider-progress-event)) events) 3)
     "non-presentational stream events still report provider progress")
    (test-assert (= (length events) 10)
     "the stream emits safe deltas, items, and completion events"))
  nil)

(defun test-provider-stream-failures ()
  "Test Responses terminal outcomes and failed streams by protocol case."
  (labels ((consume (source)
             "Consume SOURCE and return either its result or typed condition."
             (handler-case
              (provider-consume-stream (make-instance 'model-provider)
                                       (make-instance 'test-character-input-stream
                                                      :source source)
                                       nil #'identity)
              (provider-error (condition) condition))))
    (dolist
        (case
         `(("normal completion"
            ,(concatenate 'string
                          (test-sse-event-string
                           (json-object "type" "response.completed" "response"
                            (json-object "id" "completed-response")))
                          (format nil "data: [DONE]~%~%"))
            provider-result)
           ("output truncation"
            ,(test-sse-event-string
              (json-object "type" "response.incomplete" "response"
               (json-object "id" "incomplete-response" "incomplete_details"
                (json-object "reason" "max_output_tokens"))))
            provider-incomplete-response)
           ("content-filter truncation"
            ,(test-sse-event-string
              (json-object "type" "response.incomplete" "response"
               (json-object "id" "filtered-response" "incomplete_details"
                (json-object "reason" "content_filter"))))
            provider-incomplete-response)
           ("completion without response"
            ,(test-sse-event-string (json-object "type" "response.completed"))
            provider-protocol-error)
           ("completion with malformed response"
            ,(test-sse-event-string
              (json-object "type" "response.completed" "response" "invalid"))
            provider-protocol-error)
           ("incomplete without response"
            ,(test-sse-event-string (json-object "type" "response.incomplete"))
            provider-protocol-error)
           ("incomplete without reason"
            ,(test-sse-event-string
              (json-object "type" "response.incomplete" "response"
               (json-object "id" "missing-reason-response")))
            provider-protocol-error)
           ("incomplete with unknown reason"
            ,(test-sse-event-string
              (json-object "type" "response.incomplete" "response"
               (json-object "id" "unknown-reason-response" "incomplete_details"
                (json-object "reason" "future_reason"))))
            provider-protocol-error)
           ("[DONE] before terminal" ,(format nil "data: [DONE]~%~%")
            response-stream-error)
           ("clean EOF before terminal"
            ,(test-sse-event-string
              (json-object "type" "response.output_text.delta" "delta" "partial"))
            response-stream-error)
           ("failed response"
            ,(test-sse-event-string
              (json-object "type" "response.failed" "response"
               (json-object "id" "failed-response")))
            provider-error)
           ("malformed event"
            "data: {\"type\":\"response.output_text.delta\",\"delta\":\"partial"
            provider-error)))
      (destructuring-bind
          (name source expected-type)
          case
        (let ((outcome (consume source)))
          (test-assert (typep outcome expected-type)
           (format nil "Responses ~A yields ~A" name expected-type))
          (when (typep outcome 'response-stream-error)
            (test-assert (typep outcome 'provider-retryable-error)
             "unterminated Responses streams remain retryable"))))))
  nil)

(defun test-provider-stream-error-classification ()
  "Test structured SSE failures retain details and only transient codes retry."
  (let* ((source
          (test-sse-event-string
           (json-object "type" "response.failed" "response"
            (json-object "id" "failed-response" "error"
             (json-object "code" "server_error" "message" "Temporary provider failure."
              "request_id" "request-from-event")))))
         (condition
          (handler-case
           (progn
            (provider-consume-stream (make-instance 'model-provider)
                                     (make-instance 'test-character-input-stream
                                                    :source source)
                                     nil #'identity)
            nil)
           (provider-error (error) error))))
    (test-assert (typep condition 'provider-retryable-error)
     "response.failed server errors are retryable")
    (test-assert (string= (provider-error-code condition) "server_error")
     "response.failed retains its structured error code")
    (test-assert (string= (provider-error-request-id condition) "request-from-event")
     "response.failed retains its structured request identifier")
    (test-assert (string= (provider-error-response-id condition) "failed-response")
     "response.failed keeps its response identifier distinct")
    (test-assert (search "Temporary provider failure." (format nil "~A" condition))
     "response.failed surfaces the provider's explanation"))
  (let* ((source
          (test-sse-event-string
           (json-object "type" "response.completed" "response"
            (json-object "id" "usage-limit-response" "error"
             (json-object "message"
              "You've hit your usage limit. Try again tomorrow.")))))
         (condition
          (handler-case
           (progn
            (provider-consume-stream (make-instance 'model-provider)
                                     (make-instance 'test-character-input-stream
                                                    :source source)
                                     nil #'identity)
            nil)
           (provider-error (error) error))))
    (test-assert (and condition (not (typep condition 'provider-retryable-error)))
     "response.completed usage-limit failures are terminal")
    (test-assert
     (string= (provider-error-response-id condition) "usage-limit-response")
     "response.completed failures retain their response identifier")
    (test-assert (search "You've hit your usage limit" (format nil "~A" condition))
     "response.completed failures surface the provider's explanation"))
  (let* ((source
          (test-sse-event-string
           (json-object "type" "response.incomplete" "response"
            (json-object "id" "incomplete-response" "incomplete_details"
             (json-object "reason" "max_output_tokens")))))
         (condition
          (handler-case
           (progn
            (provider-consume-stream (make-instance 'model-provider)
                                     (make-instance 'test-character-input-stream
                                                    :source source)
                                     nil #'identity)
            nil)
           (provider-incomplete-response (error) error))))
    (test-assert
     (and condition (not (typep condition 'provider-retryable-error))
          (string= (provider-error-code condition) "response_incomplete")
          (string= (provider-incomplete-response-reason condition) "max_output_tokens")
          (string= (provider-error-response-id condition) "incomplete-response")
          (search "max_output_tokens" (format nil "~A" condition)))
     "response.incomplete retains its reason as a typed terminal failure"))
  (dolist (code '("server_is_overloaded" "slow_down" "rate_limit_exceeded"))
    (let* ((source
            (test-sse-event-string
             (json-object "type" "response.failed" "response"
              (json-object "id" "overloaded-response" "error"
               (json-object "code" code "message"
                "The service is temporarily overloaded.")))))
           (condition
            (handler-case
             (progn
              (provider-consume-stream (make-instance 'model-provider)
                                       (make-instance 'test-character-input-stream
                                                      :source source)
                                       nil #'identity)
              nil)
             (provider-error (error) error))))
      (test-assert (typep condition 'provider-retryable-error)
       (format nil "~A response failures are retryable" code))
      (test-assert (string= (provider-error-code condition) code)
       (format nil "~A response failures retain their code" code))
      (when (string= code "rate_limit_exceeded")
        (test-assert (provider-rate-limit-error-p condition)
         "streamed rate-limit failures retain their exhausted-allowance class"))))
  (let* ((source
          (test-sse-event-string
           (json-object "type" "error" "code" "server_error" "message"
            "Please retry the request.")))
         (condition
          (handler-case
           (progn
            (provider-consume-stream (make-instance 'model-provider)
                                     (make-instance 'test-character-input-stream
                                                    :source source)
                                     '(("x-request-id" . "request-from-header"))
                                     #'identity)
            nil)
           (provider-error (error) error))))
    (test-assert (typep condition 'provider-retryable-error)
     "top-level server error events are retryable")
    (test-assert (string= (provider-error-request-id condition) "request-from-header")
     "top-level errors fall back to the response request header")
    (test-assert (search "Please retry the request." (format nil "~A" condition))
     "top-level errors surface the provider's explanation"))
  (let* ((source
          (test-sse-event-string
           (json-object "type" "response.failed" "response"
            (json-object "id" "invalid-response" "error"
             (json-object "code" "invalid_prompt" "message"
              "The prompt is invalid.")))))
         (condition
          (handler-case
           (progn
            (provider-consume-stream (make-instance 'model-provider)
                                     (make-instance 'test-character-input-stream
                                                    :source source)
                                     nil #'identity)
            nil)
           (provider-error (error) error))))
    (test-assert
     (and (typep condition 'provider-error)
          (not (typep condition 'provider-retryable-error)))
     "invalid prompt failures remain terminal")
    (test-assert (string= (provider-error-code condition) "invalid_prompt")
     "terminal failures retain their structured error code"))
  nil)

(defun run-wire-tests ()
  "Run concrete wire fixtures."
  (let ((*wire-test-checks* 0))
    (test-provider-usage-normalization)
    (test-provider-stream-decoding)
    (test-provider-stream-failures)
    (test-provider-stream-error-classification)
    (format t "~D wire checks passed.~%" *wire-test-checks*)))
