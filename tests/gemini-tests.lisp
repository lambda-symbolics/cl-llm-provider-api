(in-package #:cl-llm-provider-api)

;;;; -- Gemini GenerateContent Wire Tests --

(defun gemini-test--provider ()
  "Return a Gemini GenerateContent provider without configuration."
  (make-instance 'gemini-generate-content-provider))

(defun gemini-test--consume (events &key (headers '(("x-request-id" . "gemini-request"))))
  "Consume EVENTS, raw SSE strings or event objects, returning the result or condition and the events."
  (let ((emitted nil))
    (values (handler-case
                (provider-consume-stream
                 (gemini-test--provider)
                 (make-string-input-stream
                  (with-output-to-string (stream)
                    (dolist (event events)
                      (write-string (if (stringp event) event (test-sse-event-string event))
                                    stream))))
                 headers
                 (lambda (event) (push event emitted)))
              (provider-error (condition)
                condition))
            (nreverse emitted))))

(defun gemini-test--request-encoding ()
  "Test contents, calls, responses, thinking, declarations, and generation options."
  (let* ((provider (gemini-test--provider))
         (tools (provider-wire-tools
                 provider
                 (json-array (json-object
                              "type" "namespace" "name" "fs" "description" "Files"
                              "tools" (json-array
                                       (json-object "name" "read" "description" "Read a file"
                                                    "parameters" (json-object "type" "object")))))))
         (wire-name (provider-wire-function-name--encode "fs" "read"))
         (request
           (provider-request-object
            provider
            (make-instance
             'wire-request
             :model "gemini-test"
             :items (list (json-object "type" "message" "role" "user" "content" "hello")
                          (json-object "type" "reasoning_content" "content" "considering")
                          (json-object "type" "function_call" "namespace" "fs" "name" "read"
                                       "call_id" "call-1" "arguments" "{\"path\":\"a\"}")
                          (json-object "type" "function_call_output" "call_id" "call-1"
                                       "output" "plain text"))
             :prefix (list "system rules" "")
             :suffix (list "keep the goal" nil "temporary context")
             :options (list :maximum-output-tokens 1024 :include-thoughts-p t))
            tools))
         (contents (json-get request "contents"))
         (parts (loop for content across contents
                      append (coerce (json-get content "parts") 'list)))
         (call (some (lambda (part) (json-get part "functionCall")) parts))
         (response (some (lambda (part) (json-get part "functionResponse")) parts))
         (trailing (aref contents (1- (length contents))))
         (generation (json-get request "generationConfig")))
    (test-assert (string= (json-get (aref (json-get (aref (json-get request "tools") 0)
                                                    "functionDeclarations")
                                          0)
                                    "name")
                          wire-name)
                 "namespaced tools become flat function declarations")
    (test-assert (and (string= (json-get call "name") wire-name)
                      (string= (json-get (json-get call "args") "path") "a")
                      (string= (json-get response "name") wire-name)
                      (string= (json-get (json-get response "response") "output") "plain text"))
                 "calls carry decoded arguments and responses name the call they answer")
    (test-assert (some (lambda (part) (json-get part "thought")) parts)
                 "reasoning replays as a thought part")
    (test-assert (and (string= (json-get (aref (json-get (json-get request "systemInstruction")
                                                         "parts")
                                               0)
                                         "text")
                               "system rules")
                      (string= (json-get trailing "role") "user")
                      (string= (json-get (aref (json-get trailing "parts") 0) "text")
                               (format nil "keep the goal~%~%temporary context")))
                 "prefix texts instruct and suffix texts trail the history as one user content")
    (test-assert (and (= (json-get generation "maxOutputTokens") 1024)
                      (eq (json-get (json-get generation "thinkingConfig") "includeThoughts") t))
                 "generation options set the output ceiling and thought inclusion")
    (test-assert (null (nth-value 1 (json-get-present
                                     (provider-request-object
                                      provider
                                      (make-instance 'wire-request :model "gemini-test")
                                      #())
                                     "generationConfig")))
                 "a request without options carries no generation configuration")))

(defun gemini-test--image-parts ()
  "Test images reach Gemini inline and unsendable images fail instead of vanishing."
  (let* ((contents (gemini--contents
                    (list (json-object
                           "type" "message" "role" "user"
                           "content" (json-array
                                      (json-object "type" "input_text" "text" "look")
                                      (json-object "type" "input_image"
                                                   "image_url" "data:image/png;base64,iVBORw0K"))))))
         (parts (json-get (aref contents 0) "parts"))
         (inline (json-get (aref parts 1) "inlineData")))
    (test-assert (and (= (length parts) 2)
                      (equal (json-get (aref parts 0) "text") "look")
                      (equal (json-get inline "mimeType") "image/png")
                      (equal (json-get inline "data") "iVBORw0K"))
                 "an image part becomes inline data beside its text")
    (test-assert (handler-case
                     (progn
                       (gemini--contents
                        (list (json-object "type" "message" "role" "user"
                                           "content" (json-array
                                                      (json-object
                                                       "type" "input_image"
                                                       "image_url" "https://example.com/a.png")))))
                       nil)
                   (provider-error (condition)
                     (equal (provider-error-code condition) "unsupported_content")))
                 "an image Gemini cannot receive is a terminal conversion failure")))

(defun gemini-test--stream-decoding ()
  "Test streamed text, thinking, calls, usage, and completion."
  (multiple-value-bind (result events)
      (gemini-test--consume
       (list (json-object
              "responseId" "response-1"
              "candidates" (json-array
                            (json-object
                             "content" (json-object
                                        "role" "model"
                                        "parts" (json-array
                                                 (json-object "text" "thinking" "thought" t)
                                                 (json-object "text" "answer")
                                                 (json-object
                                                  "functionCall"
                                                  (json-object
                                                   "name" (provider-wire-function-name--encode
                                                           "fs" "read")
                                                   "args" (json-object "path" "a")))))
                             "finishReason" "STOP"))
              "usageMetadata" (json-object "promptTokenCount" 10 "candidatesTokenCount" 4
                                           "thoughtsTokenCount" 2 "totalTokenCount" 16))))
    (let ((call (first (provider-result-tool-calls result))))
      (test-assert (and (string= (provider-result-response-id result) "response-1")
                        (= (json-get (provider-result-usage result) "input_tokens") 10)
                        (= (json-get (provider-result-usage result) "reasoning_tokens") 2))
                   "the response identifier and portable usage are kept")
      (test-assert (and (= (length (provider-result-tool-calls result)) 1)
                        (string= (json-get call "namespace") "fs")
                        (string= (json-get call "name") "read")
                        (non-empty-string-p (json-get call "call_id"))
                        (string= (json-get call "request_id") "gemini-request")
                        (eq (provider-result-turn-completion result) ':continue))
                   "a call decodes its namespace, gains an identifier, and continues the turn")
      (test-assert (and (find-if (lambda (event) (typep event 'reasoning-delta-event)) events)
                        (find-if (lambda (event) (typep event 'assistant-delta-event)) events)
                        (= (length (provider-result-output-items result)) 3))
                   "thought and answer text stream separately and become items before the call"))))

(defun gemini-test--terminal-semantics ()
  "Test finish reasons, early ends, and structured errors by protocol case."
  (flet ((candidate (finish-reason &optional text)
           "Return one stream event with optional FINISH-REASON and TEXT."
           (let ((candidate (json-object)))
             (when finish-reason
               (setf (gethash "finishReason" candidate) finish-reason))
             (when text
               (setf (gethash "content" candidate)
                     (json-object "parts" (json-array (json-object "text" text)))))
             (json-object "responseId" "gemini-response" "candidates" (json-array candidate)))))
    (dolist (case `(("normal completion"
                     ,(list (candidate "STOP" "complete") (format nil "data: [DONE]~%~%"))
                     provider-result)
                    ("output truncation" ,(list (candidate "MAX_TOKENS" "partial"))
                     provider-incomplete-response)
                    ("unknown finish reason" ,(list (candidate "FUTURE_REASON"))
                     provider-protocol-error)
                    ("[DONE] before terminal"
                     ,(list (candidate nil "partial") (format nil "data: [DONE]~%~%"))
                     response-stream-error)
                    ("clean end before terminal" ,(list (candidate nil "partial"))
                     response-stream-error)
                    ("permanent structured error"
                     ,(list (json-object "error" (json-object "status" "INVALID_ARGUMENT"
                                                              "code" 400
                                                              "message" "invalid request")))
                     provider-error)
                    ("transient structured error"
                     ,(list (json-object "error" (json-object "status" "UNAVAILABLE"
                                                              "code" 503
                                                              "message" "try again")))
                     provider-retryable-error)))
      (destructuring-bind (name events expected-type) case
        (let ((outcome (gemini-test--consume events)))
          (test-assert (typep outcome expected-type)
                       (format nil "Gemini ~A yields ~A" name expected-type))
          (when (typep outcome 'provider-incomplete-response)
            (test-assert (and (string= (provider-incomplete-response-reason outcome) "MAX_TOKENS")
                              (string= (provider-error-request-id outcome) "gemini-request")
                              (string= (provider-error-response-id outcome) "gemini-response")
                              (not (typep outcome 'provider-retryable-error)))
                         "truncation keeps its terminal metadata and is not retried"))
          (when (typep outcome 'response-stream-error)
            (test-assert (typep outcome 'provider-retryable-error)
                         "an unterminated stream stays retryable"))
          (when (string= name "permanent structured error")
            (test-assert (not (typep outcome 'provider-retryable-error))
                         "a permanent stream error does not retry"))))))
  (let ((provider (gemini-test--provider)))
    (test-assert (and (provider-retryable-status-p provider 499 nil)
                      (provider-retryable-status-p provider 599 nil)
                      (not (provider-retryable-status-p provider 498 nil)))
                 "cancelled and server error statuses are transient")))

(defun run-gemini-tests ()
  "Run the Gemini GenerateContent wire checks and return their count."
  (let ((*wire-test-checks* 0))
    (gemini-test--request-encoding)
    (gemini-test--image-parts)
    (gemini-test--stream-decoding)
    (gemini-test--terminal-semantics)
    *wire-test-checks*))
