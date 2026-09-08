(in-package #:cl-llm-provider-api)

;;;; -- Projected Requests --

(defclass wire-request ()
  ((model :initarg :model :reader wire-request-model :type string
          :documentation "The selected model identifier.")
   (items :initarg :items :reader wire-request-items :initform nil :type list
          :documentation "Already-projected portable transcript items.")
   (prefix :initarg :prefix :reader wire-request-prefix :initform nil :type list
           :documentation "Stable instruction texts preceding the history.")
   (suffix :initarg :suffix :reader wire-request-suffix :initform nil :type list
           :documentation "Request-local instruction texts following the history.")
   (options :initarg :options :reader wire-request-options :initform nil :type list
            :documentation "Wire options as a keyword property list."))
  (:documentation "An application-independent projection for a provider request."))

(defmethod provider-request-object
    ((provider responses-api-provider) (projection wire-request) (tools vector)
     &key goal-context compaction-p)
  "Encode projected Responses input, instructions, tools, and model options."
  (declare (ignore goal-context))
  (let* ((options (wire-request-options projection))
         (placement (provider-responses-instructions-placement provider))
         (prefix (wire-request-prefix projection))
         (suffix (wire-request-suffix projection))
         (wire-tools (provider-wire-tools provider tools))
         (input (append
                 (when (eq placement ':input)
                   (mapcar #'responses-developer-message
                           (remove-if-not #'non-empty-string-p prefix)))
                 (mapcar (lambda (item) (provider-wire-input-item provider item))
                         (wire-request-items projection))
                 (when (or (eq placement ':input) (not compaction-p))
                   (mapcar #'responses-developer-message
                           (remove-if-not #'non-empty-string-p suffix)))))
         (request (json-object "model" (wire-request-model projection)
                               "input" (coerce input 'vector)
                               "tools" wire-tools
                               "parallel_tool_calls" yason:false
                               "store" yason:false "stream" t)))
    (unless (member placement '(:input :top-level))
      (error 'provider-api-error
             :message (format nil "Unsupported Responses instruction placement ~S."
                              placement)))
    (when (eq placement ':top-level)
      (setf (gethash "instructions" request)
            (responses-standard-instructions
             (append prefix (and compaction-p suffix)))))
    (when (plusp (length wire-tools))
      (setf (gethash "tool_choice" request) "auto"))
    (when (getf options :reasoning-effort)
      (setf (gethash "reasoning" request)
            (apply #'json-object
                   (append (list "effort" (getf options :reasoning-effort))
                           (when (getf options :reasoning-summary)
                             (list "summary" (getf options :reasoning-summary)))))))
    (when (getf options :maximum-output-tokens)
      (setf (gethash "max_output_tokens" request)
            (getf options :maximum-output-tokens)))
    (loop for (name value) on (getf options :fields) by #'cddr
          do (setf (gethash name request) value))
    request))

(defmethod provider-request-object
    ((provider chat-completions-provider) (projection wire-request) (tools vector)
     &key goal-context compaction-p)
  "Encode projected Chat Completions history and model options."
  (declare (ignore goal-context compaction-p))
  (let* ((options (wire-request-options projection))
         (context (openai-compatible--chat-context-message
                   (wire-request-suffix projection)))
         (messages (append
                    (list (openai-compatible--chat-system-message
                           (wire-request-prefix projection)))
                    (openai-compatible--chat-input-messages
                     (wire-request-items projection))
                    (when context (list context))))
         (request (json-object "model" (wire-request-model projection)
                               "messages" (coerce messages 'vector) "stream" t)))
    (when (getf options :stream-usage-p t)
      (setf (gethash "stream_options" request)
            (json-object "include_usage" t)))
    (when (plusp (length tools))
      (setf (gethash "tools" request) tools
            (gethash "tool_choice" request) "auto"))
    (when (getf options :reasoning-parameter)
      (setf (gethash (getf options :reasoning-parameter) request)
            (getf options :reasoning-effort)))
    (when (getf options :maximum-output-tokens)
      (setf (gethash (getf options :output-ceiling-field "max_completion_tokens")
                     request)
            (getf options :maximum-output-tokens)))
    request))

;;;; -- Request Execution --

(defgeneric provider-account-label (provider)
  (:documentation "Return the provider account label for sanitized failures."))

(defmethod provider-account-label ((provider model-provider))
  "Supply a neutral account label when the host has no display metadata."
  (declare (ignore provider))
  "provider")

(defgeneric provider-note-response-headers (provider headers)
  (:documentation "Observe detached, credential-redacted HTTP response headers."))

(defmethod provider-note-response-headers ((provider model-provider) headers)
  "Ignore response metadata unless a provider specializes this hook."
  (declare (ignore provider headers))
  nil)

(defun provider-execute-request
    (provider request &key transport event-callback
                           (cleanup #'provider--close-response-stream)
                           (call-with-deadline #'funcall)
                           secrets completion)
  "Execute REQUEST using an injected TRANSPORT accepting the request object.

TRANSPORT returns a stream, HTTP status, and response headers. CLEANUP runs on
all exits after obtaining the stream. CALL-WITH-DEADLINE wraps the stream
consumer. COMPLETION runs after a valid terminal response and cleanup. Secrets
are scoped to this call and redacted before emitting detached wire values."
  (let* ((*provider-active-credential-values* secrets)
         (*provider-active-credential-redaction-marker*
           (cl-rfc8628:safe-redaction-marker
            *provider-credential-redaction-marker* secrets)))
    (multiple-value-bind (stream status raw-headers) (funcall transport request)
      (let ((result
              (unwind-protect
                   (let ((headers (provider--sanitize-wire-value raw-headers)))
                     (provider-note-response-headers provider headers)
                     (unless (= status 200)
                       (provider--signal-http-status-failure
                        provider status :headers headers :raw-body stream))
                     (funcall call-with-deadline
                              (lambda ()
                                (provider-consume-stream
                                 provider stream headers event-callback))))
                (funcall cleanup stream))))
        (when completion (funcall completion))
        result))))
