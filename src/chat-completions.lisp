(in-package #:cl-llm-provider-api)


(defun openai-compatible--wire-tool-name (namespace name)
  "Encode NAMESPACE and NAME with the shared grammar-safe provider codec."
  (let ((*provider-wire-function-name-maximum-length*
         *openai-compatible-wire-tool-name-maximum-length*))
    (provider-wire-function-name--encode namespace name)))

(defun openai-compatible--decode-wire-tool-name (wire-name)
  "Decode a namespaced Chat Completions function name, if it is ours."
  (let ((*provider-wire-function-name-maximum-length*
         *openai-compatible-wire-tool-name-maximum-length*))
    (provider-wire-function-name--decode wire-name)))

(defun openai-compatible--wire-function (name tool)
  "Return one Chat Completions function declaration for TOOL NAME."
  (json-object "name" name "description" (json-get tool "description") "parameters"
   (json-get tool "parameters") "strict" yason:false))

(defun openai-compatible--wire-tool (namespace tool)
  "Return one namespaced Autolith TOOL as a Chat Completions function tool."
  (json-object "type" "function" "function"
   (openai-compatible--wire-function
    (openai-compatible--wire-tool-name namespace (json-get tool "name")) tool)))

(defun openai-compatible--standalone-wire-tool (entry)
  "Normalize one standalone Responses function ENTRY for Chat Completions."
  (when
      (and (json-object-p entry) (json-string= (json-get entry "type") "function")
           (non-empty-string-p (json-get entry "name")))
    (json-object "type" "function" "function"
     (openai-compatible--wire-function (json-get entry "name") entry))))

(defun openai-compatible--wire-tools (tool-namespaces)
  "Flatten Autolith namespaces into standard Chat Completions tools."
  (coerce
   (loop for entry across tool-namespaces
         append (cond
                 ((and (json-object-p entry)
                       (json-string= (json-get entry "type") "namespace")
                       (non-empty-string-p (json-get entry "name"))
                       (vectorp (json-get entry "tools")))
                  (loop for tool across (json-get entry "tools")
                        when (json-object-p tool)
                        collect (openai-compatible--wire-tool (json-get entry "name")
                                 tool)))
                 (t
                  (let ((standalone (openai-compatible--standalone-wire-tool entry)))
                    (if standalone
                        (list standalone)
                        nil)))))
   'vector))

(defun openai-compatible--chat-content-part (part)
  "Translate one Responses content PART into a Chat Completions part."
  (let ((type (json-get part "type")))
    (cond
     ((and (json-string-member-p type '("input_text" "output_text" "text" "refusal"))
           (stringp (json-get part "text")))
      (json-object "type" "text" "text" (json-get part "text")))
     ((and (json-string= type "input_image")
           (non-empty-string-p (json-get part "image_url")))
      (json-object "type" "image_url" "image_url"
       (json-object "url" (json-get part "image_url"))))
     ((and (json-string= type "image_url") (non-empty-string-p (json-get part "url")))
      (json-object "type" "image_url" "image_url"
       (json-object "url" (json-get part "url"))))
     (t nil))))

(defun openai-compatible--chat-content (content)
  "Translate Responses CONTENT into a Chat Completions content value."
  (cond ((stringp content) content)
        ((vectorp content)
         (coerce
          (loop for part across content
                for translated = (and (json-object-p part)
                                      (openai-compatible--chat-content-part part))
                when translated
                collect translated)
          'vector))
        ((null content) "") (t (bounded-string content :limit 2000))))

(defun openai-compatible--wire-call-name (item)
  "Return the flat Chat Completions function name for function-call ITEM."
  (let ((namespace (json-get item "namespace")) (name (json-get item "name")))
    (if (non-empty-string-p namespace)
        (openai-compatible--wire-tool-name namespace name)
        name)))

(defun openai-compatible--chat-message (item)
  "Translate a Responses message ITEM into a Chat Completions message."
  (let ((role (json-get item "role")))
    (when (json-string-member-p role '("user" "assistant" "developer" "system"))
      (json-object "role"
       (if (json-string-member-p role '("developer" "system"))
           "system"
           role)
       "content" (openai-compatible--chat-content (json-get item "content"))))))

(defun openai-compatible--chat-function-call-entry (item)
  "Translate one Responses function-call ITEM into a Chat tool-call entry."
  (json-object "id" (json-get item "call_id") "type" "function" "function"
   (json-object "name" (openai-compatible--wire-call-name item) "arguments"
    (or (json-get item "arguments") "{}"))))

(defun openai-compatible--chat-function-calls (items)
  "Translate consecutive Responses function-call ITEMS into one assistant message."
  (json-object "role" "assistant" "content" nil "tool_calls"
   (apply #'json-array (mapcar #'openai-compatible--chat-function-call-entry items))))

(defun openai-compatible--chat-tool-output (item)
  "Translate a Responses function-call output ITEM into a tool message."
  (json-object "role" "tool" "tool_call_id" (json-get item "call_id") "content"
   (openai-compatible--chat-content (json-get item "output"))))

(defun openai-compatible--chat-input-item (item)
  "Translate one portable Responses input ITEM for Chat Completions."
  (when (json-object-p item)
    (cond
     ((json-string= (json-get item "type") "message")
      (openai-compatible--chat-message item))
     ((json-string= (json-get item "type") "function_call_output")
      (openai-compatible--chat-tool-output item))
     (t nil))))

(defun openai-compatible--chat-input-messages (items)
  "Translate portable Responses ITEMS into valid Chat Completions messages.

Captured thinking rides on the same round's tool-call assistant
message, which thinking-mode providers require passed back."
  (let ((messages nil) (function-calls nil) (pending-reasoning nil))
    (labels ((flush-function-calls ()
               "Append one assistant message for pending function calls."
               (when function-calls
                 (let ((message
                        (openai-compatible--chat-function-calls
                         (nreverse function-calls))))
                   (when pending-reasoning
                     (setf (gethash "reasoning_content" message) pending-reasoning
                           pending-reasoning nil))
                   (push message messages))
                 (setf function-calls nil))))
      (dolist (item items)
        (cond
         ((clinker-transcript:chat-reasoning-item-p item)
          (setf pending-reasoning (json-get item "content")))
         ((and (json-object-p item) (clinker-transcript:function-call-item-p item))
          (push item function-calls))
         (t (flush-function-calls)
          (when (and (json-object-p item) (json-string= (json-get item "role") "user"))
            (setf pending-reasoning nil))
          (let ((message (openai-compatible--chat-input-item item)))
            (when message (push message messages))))))
      (flush-function-calls)
      (nreverse messages))))

(defun openai-compatible--chat-system-message (texts)
  "Return one leading Chat Completions system message joining nonempty TEXTS."
  (json-object "role" "system" "content"
   (format nil "~{~A~^~%~%~}" (remove-if-not #'non-empty-string-p texts))))

(defun openai-compatible--chat-context-message (texts)
  "Return volatile nonempty TEXTS as one trailing Chat Completions user message."
  (let ((content
         (format nil "~{~A~^~%~%~}" (remove-if-not #'non-empty-string-p texts))))
    (when (non-empty-string-p content) (json-object "role" "user" "content" content))))

(defun openai-compatible--delta-text (value)
  "Return visible text from a Chat Completions delta VALUE."
  (cond ((stringp value) value)
        ((vectorp value)
         (let ((parts
                (loop for part across value
                      when (and (json-object-p part) (stringp (json-get part "text")))
                      collect (json-get part "text"))))
           (and parts (format nil "~{~A~^~%~}" parts))))
        (t nil)))

(defstruct
    (openai-compatible-tool-state (:constructor openai-compatible--tool-state (index)))
  "Mutable accumulator for one Chat Completions tool call."
  (index 0 :type (integer 0) :read-only t)
  (id nil :type (option string))
  (name-stream (make-string-output-stream) :type stream :read-only t)
  (name-character-count 0 :type (integer 0))
  (arguments-stream (make-string-output-stream) :type stream :read-only t)
  (arguments-character-count 0 :type (integer 0)))

(defun openai-compatible--append-tool-delta (states index delta)
  "Merge one Chat Completions tool DELTA into STATES and return its state."
  (let ((state
         (or (gethash index states)
             (setf (gethash index states) (openai-compatible--tool-state index)))))
    (let* ((function (and (json-object-p delta) (json-get delta "function")))
           (id (and (json-object-p delta) (json-get delta "id")))
           (name
            (or (and (json-object-p function) (json-get function "name"))
                (and (json-object-p delta) (json-get delta "name"))))
           (arguments
            (or (and (json-object-p function) (json-get function "arguments"))
                (and (json-object-p delta) (json-get delta "arguments")))))
      (when (non-empty-string-p id) (setf (openai-compatible-tool-state-id state) id))
      (when (non-empty-string-p name)
        (write-string name (openai-compatible-tool-state-name-stream state))
        (incf (openai-compatible-tool-state-name-character-count state) (length name)))
      (when (stringp arguments)
        (write-string arguments (openai-compatible-tool-state-arguments-stream state))
        (incf (openai-compatible-tool-state-arguments-character-count state)
              (length arguments))))
    state))

(defun openai-compatible--tool-delta-index (tool fallback-index)
  "Return TOOL's wire index, or FALLBACK-INDEX when it declares no valid one."
  (let ((index (json-get tool "index")))
    (if (and (integerp index) (not (minusp index)))
        index
        fallback-index)))

(defun openai-compatible--choice-tool-deltas (delta)
  "Return validated indexed tool DELTA pairs from one Chat Completions delta."
  (let ((tool-deltas (json-get delta "tool_calls")))
    (if (vectorp tool-deltas)
        (loop for tool across tool-deltas
              for fallback-index from 0
              when (json-object-p tool)
              collect (list (openai-compatible--tool-delta-index tool fallback-index)
                            tool))
        (let ((function-call (json-get delta "function_call")))
          (if (json-object-p function-call)
              (list (list 0 function-call))
              nil)))))

(defun openai-compatible--stream-tool-states (states)
  "Return accumulated tool STATES in wire index order."
  (sort
   (loop for state being the hash-values of states
         collect state)
   #'< :key #'openai-compatible-tool-state-index))

(defun openai-compatible--function-call-item (state headers)
  "Return one complete normalized Responses function-call item from STATE."
  (let ((id (openai-compatible-tool-state-id state))
        (name
         (get-output-stream-string (openai-compatible-tool-state-name-stream state)))
        (arguments
         (get-output-stream-string
          (openai-compatible-tool-state-arguments-stream state))))
    (unless (and (non-empty-string-p id) (non-empty-string-p name))
      (provider--signal-stream-protocol-failure headers
       "The provider returned an incomplete tool call."))
    (json-object "type" "function_call" "call_id" id "name" name "arguments"
     (if (zerop (length arguments))
         "{}"
         arguments))))

(defun openai-compatible--fallback-tool-name (name)
  "Split a canonical dotted tool NAME a model echoed instead of ours.

Models that do not repeat the encoded function names verbatim commonly
reproduce the dotted canonical name from the prompt, sometimes with a
stray leading dot. A bare name without a namespace half is left for
the registry's unique-name dispatch."
  (let* ((trimmed (string-left-trim "." name))
         (separator (position #\FULL_STOP trimmed)))
    (if (and separator (plusp separator) (< (1+ separator) (length trimmed)))
        (values (subseq trimmed 0 separator) (subseq trimmed (1+ separator)))
        (values nil (and (plusp (length trimmed)) trimmed)))))

(defmethod provider-normalize-output-item
           ((provider chat-completions-provider) (item hash-table))
  "Strip server identifiers and split encoded names into namespaced calls."
  (call-next-method)
  (when (clinker-transcript:function-call-item-p item)
    (let ((name (json-get item "name")))
      (when (stringp name)
        (multiple-value-bind (namespace tool-name)
            (openai-compatible--decode-wire-tool-name name)
          (unless (and namespace tool-name)
            (multiple-value-setq (namespace tool-name)
              (openai-compatible--fallback-tool-name name)))
          (when tool-name
            (when namespace (setf (gethash "namespace" item) namespace))
            (setf (gethash "name" item) tool-name))))))
  item)

(defmethod provider-consume-stream
           ((provider chat-completions-provider) stream headers event-callback)
  "Consume a Chat Completions SSE stream into a provider result."
  (let ((response-id nil)
        (usage nil)
        (finish-reason nil)
        (text-stream (make-string-output-stream))
        (reasoning-stream (make-string-output-stream))
        (tool-states (make-hash-table :test #'equal))
        (completed-p nil))
    (loop until completed-p
          for data = (provider--read-sse-data stream headers)
          do (cond
              ((eq data *sse-end-of-stream*)
               (provider--signal-stream-interruption headers
                "The provider stream closed before the [DONE] marker."))
              ((string= data "[DONE]")
               (if finish-reason
                   (setf completed-p t)
                   (provider--signal-stream-interruption headers
                    "The provider stream ended before a validated finish reason.")))
              (t
               (let* ((event (provider--decode-sse-data data headers))
                      (error-object
                       (and (json-object-p event)
                            (provider--event-error-object event))))
                 (when error-object
                   (provider--signal-event-failure event :type "error" :data data
                    :headers headers :response-id response-id))
                 (when (json-object-p event)
                   (let ((event-id (json-get event "id"))
                         (event-usage (json-get event "usage")))
                     (when (non-empty-string-p event-id) (setf response-id event-id))
                     (when event-usage (setf usage event-usage)))
                   (let ((choices (json-get event "choices")))
                     (if (vectorp choices)
                         (loop for choice across choices
                               when (json-object-p choice)
                               do (let* ((delta
                                          (or (json-get choice "delta")
                                              (json-get choice "message")))
                                         (text
                                          (and (json-object-p delta)
                                               (openai-compatible--delta-text
                                                (json-get delta "content"))))
                                         (reasoning
                                          (and (json-object-p delta)
                                               (or (json-get delta "reasoning_content")
                                                   (json-get delta "reasoning")
                                                   (json-get delta "thinking"))))
                                         (finish (json-get choice "finish_reason")))
                                    (when text
                                      (write-string text text-stream)
                                      (funcall event-callback
                                               (make-instance 'assistant-delta-event
                                                              :text text)))
                                    (when (stringp reasoning)
                                      (write-string reasoning reasoning-stream)
                                      (funcall event-callback
                                               (make-instance 'reasoning-delta-event
                                                              :text reasoning)))
                                    (when (json-object-p delta)
                                      (dolist
                                          (tool-delta
                                           (openai-compatible--choice-tool-deltas
                                            delta))
                                        (openai-compatible--append-tool-delta
                                         tool-states (first tool-delta)
                                         (second tool-delta))))
                                    (when finish
                                      (unless (non-empty-string-p finish)
                                        (provider--signal-invalid-terminal-reason
                                         finish headers :response-id response-id))
                                      (cond
                                       ((member finish
                                                '("length" "max_tokens"
                                                  "max_output_tokens"
                                                  "model_context_window_exceeded")
                                                :test #'string=)
                                        (provider--signal-incomplete-terminal finish
                                         headers :response-id response-id))
                                       ((member finish
                                                '("stop" "tool_calls" "function_call")
                                                :test #'string=)
                                        (setf finish-reason finish))
                                       (t
                                        (provider--signal-invalid-terminal-reason
                                         finish headers :response-id
                                         response-id))))) (funcall event-callback
                                                                   (make-instance
                                                                    'provider-progress-event))))))))))
    (let ((output-items nil)
          (reasoning-text (get-output-stream-string reasoning-stream))
          (assistant-text (get-output-stream-string text-stream)))
      (when (plusp (length reasoning-text))
        (push (json-object "type" "reasoning_content" "content" reasoning-text)
              output-items))
      (when (plusp (length assistant-text))
        (push
         (json-object "type" "message" "role" "assistant" "content"
          (json-array (json-object "type" "output_text" "text" assistant-text)))
         output-items))
      (let* ((states (openai-compatible--stream-tool-states tool-states))
             (tool-call-p
              (some
               (lambda (state)
                 (plusp (openai-compatible-tool-state-name-character-count state)))
               states)))
        (dolist (state states)
          (let ((item (openai-compatible--function-call-item state headers)))
            (provider-normalize-output-item provider item)
            (push item output-items)))
        (setf output-items (nreverse output-items))
        (dolist (item output-items)
          (funcall event-callback (make-instance 'provider-item-event :item item)))
        (let ((turn-completion
               (if (or tool-call-p
                       (member finish-reason '("tool_calls" "function_call") :test
                               #'string=))
                   ':continue
                   ':end)))
          (funcall event-callback
                   (make-instance 'provider-completed-event :response-id response-id
                                  :usage usage :turn-completion turn-completion))
          (make-instance 'provider-result :response-id response-id :output-items
                         output-items :tool-calls
                         (remove-if-not #'clinker-transcript:function-call-item-p
                                        output-items)
                         :usage usage :turn-state nil :turn-completion
                         turn-completion))))))

(defmethod provider-wire-tools ((provider chat-completions-provider) (tools vector))
  "Encode projected namespaces as Chat Completions function declarations."
  (declare (ignore provider))
  (openai-compatible--wire-tools tools))
