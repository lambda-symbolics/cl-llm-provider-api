(in-package #:cl-llm-provider-api)

;;;; -- Gemini GenerateContent Wire Protocol --

(defclass gemini-generate-content-provider (model-provider)
  ()
  (:documentation "A streaming Gemini GenerateContent client over projected request data."))

(defgeneric provider-gemini-stream-response (provider event)
  (:documentation
   "Return the GenerateContent response carried by stream EVENT and its response identifier.

Hosts that wrap each response in an envelope, as Gemini Code Assist does,
specialize this to unwrap it."))

(defmethod provider-gemini-stream-response
    ((provider gemini-generate-content-provider) event)
  "Read a plain GenerateContent stream EVENT as the response itself."
  (declare (ignore provider))
  (values event (json-get event "responseId")))

(defmethod provider-wire-protocol ((provider gemini-generate-content-provider))
  "Identify the Gemini GenerateContent wire protocol."
  (declare (ignore provider))
  ':gemini-generate-content)

(defmethod provider-output-ceiling-p ((provider gemini-generate-content-provider))
  "GenerateContent's generationConfig accepts maxOutputTokens."
  (declare (ignore provider))
  t)

(defmethod provider-retryable-status-p
    ((provider gemini-generate-content-provider) (status integer) headers)
  "Retry Google's cancelled status and every server error as transient."
  (or (call-next-method)
      (= status 499)
      (<= 500 status 599)))

(defmethod provider-wire-tool-name
    ((provider gemini-generate-content-provider) (namespace string) (name string))
  "Encode one flat, reversible Gemini function name."
  (declare (ignore provider))
  (provider-wire-function-name--encode namespace name))

(defmethod provider-wire-tools
    ((provider gemini-generate-content-provider) (tool-namespaces vector))
  "Flatten projected tool namespaces into Gemini function declarations."
  (declare (ignore provider))
  (gemini--function-declarations tool-namespaces))

(defmethod provider-request-object
    ((provider gemini-generate-content-provider) (projection wire-request) (tools vector)
     &key goal-context compaction-p)
  "Encode projected contents, instruction texts, declarations, and generation options.

PREFIX texts form the systemInstruction, history ITEMS become contents, and
SUFFIX texts trail them as one user content. TOOLS is the result of
PROVIDER-WIRE-TOOLS. Options are :MAXIMUM-OUTPUT-TOKENS and :INCLUDE-THOUGHTS-P."
  (declare (ignore provider goal-context compaction-p))
  (let* ((options (wire-request-options projection))
         (contents (gemini--contents (wire-request-items projection)))
         (trailing (gemini--text-content (wire-request-suffix projection)))
         (system (gemini--text-content (wire-request-prefix projection)))
         (request (json-object "contents" (if trailing
                                               (concatenate 'vector contents (vector trailing))
                                               contents)))
         (generation (json-object)))
    (when system
      (setf (gethash "systemInstruction" request) system))
    (when (plusp (length tools))
      (setf (gethash "tools" request)
            (json-array (json-object "functionDeclarations" (map 'vector #'json-object-copy tools)))))
    (when (getf options :maximum-output-tokens)
      (setf (gethash "maxOutputTokens" generation) (getf options :maximum-output-tokens)))
    (when (getf options :include-thoughts-p)
      (setf (gethash "thinkingConfig" generation) (json-object "includeThoughts" t)))
    (when (plusp (hash-table-count generation))
      (setf (gethash "generationConfig" request) generation))
    request))

(defmethod provider-consume-stream
    ((provider gemini-generate-content-provider) stream headers event-callback)
  "Consume a GenerateContent SSE stream into a provider result.

Thought text streams as reasoning deltas and other text as assistant deltas.
STOP completes the response; MAX_TOKENS and MODEL_CONTEXT_WINDOW_EXCEEDED
signal an incomplete response, any other finish reason a protocol error, and an
error object the typed failure it describes. A stream that ends before a finish
reason is a retryable interruption."
  (let ((response-id nil)
        (usage nil)
        (text-stream (make-string-output-stream))
        (reasoning-stream (make-string-output-stream))
        (calls nil)
        (call-index 0)
        (completed-p nil))
    (loop until completed-p
          for data = (provider--read-sse-data stream headers)
          do (cond
               ((eq data *sse-end-of-stream*)
                (provider--signal-stream-interruption
                 headers "The provider stream closed before a validated finish reason."))
               ((string= data "[DONE]")
                (provider--signal-stream-interruption
                 headers "The provider stream ended before a validated finish reason."))
               (t
                (let ((event (provider--decode-sse-data data headers)))
                  (when (json-object-p event)
                    (when (json-object-p (json-get event "error"))
                      (provider--signal-event-failure event :type "error" :data data
                                                            :headers headers
                                                            :response-id response-id))
                    (multiple-value-bind (response event-response-id)
                        (provider-gemini-stream-response provider event)
                      (when (non-empty-string-p event-response-id)
                        (setf response-id event-response-id))
                      (when (json-object-p response)
                        (let ((metadata (json-get response "usageMetadata")))
                          (when (json-object-p metadata)
                            (setf usage (gemini--usage metadata))))
                        (let ((candidates (json-get response "candidates")))
                          (when (vectorp candidates)
                            (loop for candidate across candidates
                                  when (json-object-p candidate)
                                    do (when (gemini--candidate-finished-p
                                              (json-get candidate "finishReason")
                                              headers response-id)
                                         (setf completed-p t))
                                       (dolist (part (gemini--candidate-parts candidate))
                                         (let ((text (json-get part "text"))
                                               (function-call (json-get part "functionCall")))
                                           (when (stringp text)
                                             (if (json-get part "thought")
                                                 (progn
                                                   (write-string text reasoning-stream)
                                                   (funcall event-callback
                                                            (make-instance 'reasoning-delta-event
                                                                           :text text)))
                                                 (progn
                                                   (write-string text text-stream)
                                                   (funcall event-callback
                                                            (make-instance 'assistant-delta-event
                                                                           :text text)))))
                                           (when (json-object-p function-call)
                                             (push (gemini--normalize-call
                                                    provider function-call (incf call-index)
                                                    headers)
                                                   calls))))))))))))))
    (gemini--finish-result
     :response-id response-id
     :usage usage
     :reasoning (get-output-stream-string reasoning-stream)
     :text (get-output-stream-string text-stream)
     :calls (nreverse calls)
     :event-callback event-callback)))

(defun gemini--signal-request-conversion-failure (message)
  "Signal a terminal Gemini request conversion failure described by MESSAGE."
  (error 'provider-error
         :message message
         :status nil
         :code "unsupported_content"
         :request-id nil
         :response-id nil
         :response nil))

(defun gemini--wire-name (item)
  "Return portable call ITEM's flat Gemini function name."
  (let ((namespace (json-get item "namespace"))
        (name (json-get item "name")))
    (if (non-empty-string-p namespace)
        (provider-wire-function-name--encode namespace name)
        name)))

(defun gemini--decoded-object (value)
  "Return VALUE when it is a JSON object or a string encoding one, otherwise NIL."
  (cond
    ((json-object-p value)
     value)
    ((stringp value)
     (let ((decoded (handler-case (json-decode value)
                      (error ()
                        nil))))
       (and (json-object-p decoded) decoded)))
    (t
     nil)))

(defun gemini--call-arguments (arguments)
  "Return call ARGUMENTS as Gemini args, wrapping a non-object string under value."
  (or (gemini--decoded-object arguments)
      (if (stringp arguments)
          (json-object "value" arguments)
          (json-object))))

(defun gemini--function-response (output)
  "Return tool OUTPUT as a Gemini functionResponse response object."
  (or (gemini--decoded-object output)
      (json-object "output" output)))

(defun gemini--image-part (part)
  "Translate one portable input_image PART into a Gemini inlineData part.

Gemini takes an image inline as its MIME type and base64 data, which the
portable data URL carries. Any other image cannot be sent and signals a
terminal PROVIDER-ERROR instead of being dropped from the request."
  (let* ((image-url (json-get part "image_url"))
         (url (if (json-object-p image-url) (json-get image-url "url") image-url))
         (marker (and (stringp url) (search ";base64," url))))
    (unless (and marker
                 (eql 0 (search "data:" url))
                 (> marker (length "data:")))
      (gemini--signal-request-conversion-failure
       "Gemini can only receive images given as base64 data URLs."))
    (json-object "inlineData"
                 (json-object "mimeType" (subseq url (length "data:") marker)
                              "data" (subseq url (+ marker (length ";base64,")))))))

(defun gemini--message-parts (content)
  "Translate portable message CONTENT into Gemini text and inline image parts."
  (coerce
   (cond
     ((stringp content)
      (list (json-object "text" content)))
     ((vectorp content)
      (loop for part across content
            when (json-object-p part)
              append (let ((type (json-get part "type"))
                           (text (json-get part "text")))
                       (cond
                         ((json-string= type "input_image")
                          (list (gemini--image-part part)))
                         ((and (stringp text)
                               (or (plusp (length text))
                                   (json-string-member-p type '("input_text" "output_text"))))
                          (list (json-object "text" text)))
                         (t
                          nil)))))
     (t
      (list (json-object "text" (bounded-string content :limit 2000)))))
   'vector))

(defun gemini--contents (items)
  "Translate portable conversation ITEMS into Gemini contents.

Function responses carry the name of the call they answer, found by call_id."
  (let ((contents nil)
        (call-names (make-hash-table :test #'equal)))
    (dolist (item items)
      (when (json-object-p item)
        (let ((type (json-get item "type")))
          (cond
            ((json-string= type "message")
             (let ((role (json-get item "role")))
               (when (json-string-member-p role '("user" "assistant"))
                 (push (json-object "role" (if (string= role "assistant") "model" "user")
                                    "parts" (gemini--message-parts (json-get item "content")))
                       contents))))
            ((clinker-transcript:function-call-item-p item)
             (let ((name (gemini--wire-name item)))
               (setf (gethash (json-get item "call_id") call-names) name)
               (push (json-object
                      "role" "model"
                      "parts" (json-array
                               (json-object
                                "functionCall"
                                (json-object "name" name
                                             "args" (gemini--call-arguments
                                                     (json-get item "arguments"))))))
                     contents)))
            ((json-string= type "function_call_output")
             (push (json-object
                    "role" "user"
                    "parts" (json-array
                             (json-object
                              "functionResponse"
                              (json-object "name" (or (gethash (json-get item "call_id") call-names)
                                                      (json-get item "name")
                                                      "unknown_function")
                                           "response" (gemini--function-response
                                                       (json-get item "output"))))))
                   contents))
            ((json-string= type "reasoning_content")
             (push (json-object "role" "model"
                                "parts" (json-array
                                         (json-object "text" (or (json-get item "content") "")
                                                      "thought" t)))
                   contents))))))
    (coerce (nreverse contents) 'vector)))

(defun gemini--text-content (texts)
  "Return the nonempty TEXTS joined into one user content, or NIL when none remain."
  (let ((text (format nil "~{~A~^~%~%~}" (remove-if-not #'non-empty-string-p texts))))
    (when (plusp (length text))
      (json-object "role" "user" "parts" (json-array (json-object "text" text))))))

(defun gemini--function-declarations (tool-namespaces)
  "Flatten namespaces and plain functions in TOOL-NAMESPACES into Gemini declarations."
  (coerce
   (loop for entry across tool-namespaces
         when (json-object-p entry)
           append (let ((type (json-get entry "type")))
                    (cond
                      ((and (json-string= type "namespace")
                            (non-empty-string-p (json-get entry "name"))
                            (vectorp (json-get entry "tools")))
                       (loop for tool across (json-get entry "tools")
                             when (json-object-p tool)
                               collect (json-object
                                        "name" (provider-wire-function-name--encode
                                                (json-get entry "name") (json-get tool "name"))
                                        "description" (json-get tool "description")
                                        "parameters" (json-get tool "parameters"))))
                      ((json-string= type "function")
                       (list (json-object "name" (json-get entry "name")
                                          "description" (json-get entry "description")
                                          "parameters" (json-get entry "parameters"))))
                      (t
                       nil))))
   'vector))

(defun gemini--candidate-finished-p (finish-reason headers response-id)
  "Return true when FINISH-REASON completes the response; signal for failing reasons."
  (cond
    ((null finish-reason)
     nil)
    ((not (non-empty-string-p finish-reason))
     (provider--signal-invalid-terminal-reason finish-reason headers :response-id response-id))
    ((member finish-reason '("MAX_TOKENS" "MODEL_CONTEXT_WINDOW_EXCEEDED") :test #'string=)
     (provider--signal-incomplete-terminal finish-reason headers :response-id response-id))
    ((string= finish-reason "STOP")
     t)
    (t
     (provider--signal-invalid-terminal-reason finish-reason headers :response-id response-id))))

(defun gemini--candidate-parts (candidate)
  "Return CANDIDATE's content parts that are JSON objects."
  (let* ((content (json-get candidate "content"))
         (parts (and (json-object-p content) (json-get content "parts"))))
    (and (vectorp parts)
         (remove-if-not #'json-object-p (coerce parts 'list)))))

(defun gemini--call-identifier (index)
  "Return a fresh call identifier for the INDEXth call of a response that named none."
  (format nil "gemini-call-~D-~36R" index (random (expt 2 64) (make-random-state t))))

(defun gemini--normalize-call (provider function-call index headers)
  "Return the portable function_call item for Gemini FUNCTION-CALL."
  (let* ((wire-name (json-get function-call "name"))
         (item (json-object "type" "function_call"
                            "call_id" (or (json-get function-call "id")
                                          (gemini--call-identifier index))
                            "name" wire-name
                            "arguments" (json-encode (or (json-get function-call "args")
                                                         (json-object))))))
    (multiple-value-bind (namespace name) (provider-wire-function-name--decode wire-name)
      (when (and namespace name)
        (setf (gethash "namespace" item) namespace
              (gethash "name" item) name)))
    (when (provider--response-request-id headers)
      (setf (gethash "request_id" item) (provider--response-request-id headers)))
    (provider-normalize-output-item provider item)))

(defun gemini--usage (metadata)
  "Translate Gemini usage METADATA into portable usage counters."
  (let ((usage (json-object)))
    (loop for (wire-name . name) in '(("promptTokenCount" . "input_tokens")
                                      ("candidatesTokenCount" . "output_tokens")
                                      ("totalTokenCount" . "total_tokens")
                                      ("cachedContentTokenCount" . "cached_input_tokens")
                                      ("thoughtsTokenCount" . "reasoning_tokens"))
          for value = (json-get metadata wire-name)
          when (typep value '(integer 0))
            do (setf (gethash name usage) value))
    usage))

(defun gemini--finish-result (&key response-id usage reasoning text calls event-callback)
  "Emit the completed items and return the provider result of one Gemini response.

REASONING and TEXT become one reasoning and one assistant message item ahead
of the CALLS; a response with calls continues the turn."
  (let ((items (append
                (when (plusp (length reasoning))
                  (list (json-object "type" "reasoning_content" "content" reasoning)))
                (when (plusp (length text))
                  (list (json-object "type" "message" "role" "assistant"
                                     "content" (json-array
                                                (json-object "type" "output_text"
                                                             "text" text)))))
                calls))
        (turn-completion (if calls ':continue ':end)))
    (dolist (item items)
      (funcall event-callback (make-instance 'provider-item-event :item item)))
    (funcall event-callback (make-instance 'provider-completed-event
                                           :response-id response-id
                                           :usage usage
                                           :turn-completion turn-completion))
    (make-instance 'provider-result
                   :response-id response-id
                   :output-items items
                   :tool-calls calls
                   :usage usage
                   :turn-state nil
                   :turn-completion turn-completion)))
