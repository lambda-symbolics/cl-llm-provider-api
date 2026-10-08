(in-package #:cl-llm-provider-api)

;;;; -- GitHub Copilot wire/catalog helpers --

(defun copilot--error (format-control &rest arguments)
  "Signal a typed error for malformed Copilot wire configuration."
  (error 'provider-error :message (apply #'format nil format-control arguments)))

(defun copilot--field (object &rest keys)
  "Read nested JSON KEYS, retaining the distinction between false and absent."
  (dolist (key keys)
    (unless (json-object-p object)
      (return-from copilot--field nil))
    (setf object (gethash key object)))
  object)

(defun copilot-model-protocol (entry)
  "Return the supported Copilot wire protocol for catalog ENTRY."
  (unless (json-object-p entry)
    (copilot--error "Copilot model entry must be a JSON object."))
  (multiple-value-bind (endpoints endpoints-p) (gethash "supported_endpoints" entry)
    (let ((id (json-get entry "id")))
      (when (and endpoints-p
                 (not (and (vectorp endpoints) (not (stringp endpoints))
                           (every #'stringp endpoints))))
        (copilot--error "Copilot supported_endpoints must be an array of strings."))
      (when (and id (not (stringp id)))
        (copilot--error "Copilot model id must be a string."))
      (cond
        ((find "/v1/messages" endpoints :test #'equal) :messages)
        ((find "/messages" endpoints :test #'equal) :messages)
        ((find "/responses" endpoints :test #'equal) :responses)
        ((find "/chat/completions" endpoints :test #'equal) :chat-completions)
        (endpoints-p :unsupported)
        ((and id (uiop:string-prefix-p "claude-" id)) :messages)
        ((and id (some (lambda (prefix) (uiop:string-prefix-p prefix id))
                       (list "gpt-5" "gpt-6" "grok-" "mai-"))) :responses)
        (t :chat-completions)))))

(defun copilot--catalog-entries (document)
  "Validate DOCUMENT and return concrete tool-capable routes, including hidden models."
  (unless (json-object-p document)
    (copilot--error "Copilot model catalog must be a JSON object."))
  (let ((data (json-get document "data")))
    (unless (and (vectorp data) (not (stringp data)))
      (copilot--error "Copilot model catalog data must be an array."))
    (loop for entry across data
          do (unless (and (json-object-p entry)
                          (non-empty-string-p (json-get entry "id")))
               (copilot--error "Copilot catalog entries require a model ID."))
          unless (or (string= (json-get entry "id") "auto")
                     (member (copilot--field entry "capabilities" "supports" "tool_calls")
                             (list *json-decoded-false* yason:false))
                     (eq (copilot-model-protocol entry) :unsupported))
            collect entry)))

(defun copilot--model-spec (entry model-prefix &key automatic-p)
  "Decode a validated concrete ENTRY for manual or automatic selection."
  (let ((id (json-get entry "id"))
        (name (json-get entry "name"))
        (window (copilot--field entry "capabilities" "limits" "max_prompt_tokens")))
    (when (and name (not (stringp name)))
      (copilot--error "Copilot model descriptions must be strings."))
    (append (list :name (concatenate 'string model-prefix id)
                  :protocol (copilot-model-protocol entry)
                  :description (or name id)
                  :reasoning-efforts
                  (if (and (not automatic-p)
                           (eq (copilot--field entry "capabilities" "supports" "reasoning_effort") t))
                      '("low" "medium" "high") '("none")))
            (when (typep window '(integer 1)) (list :context-window window)))))

(defun copilot-model-catalog (document &key (model-prefix "") personal-account-p
                                          enable-model-function include-auto-p)
  "Decode picker models, optionally enabling eligible entries and including Auto.
The enable callback receives one unique eligible unconfigured model ID. Hidden
routes authorize Auto selection only when a session explicitly offers them."
  (unless (stringp model-prefix)
    (copilot--error "Copilot model namespace must be a string."))
  (let* ((entries (copilot--catalog-entries document))
         (picker-p (some (lambda (entry)
                           (and (eq (json-get entry "model_picker_enabled") t)
                                (not (equal (copilot--field entry "policy" "state") "disabled"))))
                         entries))
         (fallback-p (and personal-account-p (not picker-p)))
         (seen nil)
         (models nil))
    (dolist (entry entries)
      (let* ((id (json-get entry "id"))
             (policy (copilot--field entry "policy" "state"))
             (eligible-p (or (eq (json-get entry "model_picker_enabled") t) fallback-p)))
        (when (and eligible-p (not (member id seen :test #'string=)))
          (push id seen)
          (when (or (equal policy "enabled")
                    (and (null policy) (not fallback-p))
                    (and (equal policy "unconfigured") enable-model-function
                         (funcall enable-model-function id)))
            (push (copilot--model-spec entry model-prefix) models)))))
    (let ((result (nreverse models)))
      (if (and include-auto-p entries)
          (let* ((windows (loop for entry in entries
                                for limit = (copilot--field entry "capabilities" "limits" "max_prompt_tokens")
                                when (typep limit '(integer 1)) collect limit))
                 (window (and windows (reduce #'min windows))))
            (cons (append (list :name (concatenate 'string model-prefix "auto")
                                :protocol :auto :description "Auto"
                                :reasoning-efforts '("none"))
                          (when window (list :context-window window)))
                  result))
          result))))

(defun copilot--valid-host-p (host)
  "Return true for a plain DNS host without URL syntax or credentials."
  (and (stringp host) (<= 1 (length host) 253)
       (every (lambda (label)
                (and (<= 1 (length label) 63)
                     (not (char= (char label 0) #\-))
                     (not (char= (char label (1- (length label))) #\-))
                     (every (lambda (character)
                              (or (find character "abcdefghijklmnopqrstuvwxyz0123456789-")
                                  nil))
                            label)))
              (uiop:split-string host :separator '(#\.)))))

(defun copilot-base-url (token &key (domain "github.com"))
  "Return the trusted HTTPS Copilot API base URL for TOKEN and DOMAIN."
  (unless (and (non-empty-string-p token) (copilot--valid-host-p domain))
    (copilot--error "Copilot token or issuer domain is invalid."))
  (let ((proxy (loop for field in (uiop:split-string (or token "") :separator '(#\;))
                     when (uiop:string-prefix-p "proxy-ep=" field)
                     return (subseq field (length "proxy-ep=")))))
    (if proxy
        (progn
          (unless (and (uiop:string-prefix-p "proxy." proxy)
                       (copilot--valid-host-p proxy)
                       (or (uiop:string-suffix-p proxy ".githubcopilot.com")
                           (and (not (string= domain "github.com"))
                                (uiop:string-suffix-p proxy
                                                       (concatenate 'string "." domain)))))
            (copilot--error "Copilot returned an untrusted proxy endpoint."))
          (concatenate 'string "https://api." (subseq proxy (length "proxy."))))
        (if (string= domain "github.com")
            "https://api.individual.githubcopilot.com"
            (format nil "https://copilot-api.~A" domain)))))

(defun copilot-protocol-endpoint (protocol)
  "Return the endpoint path for Copilot PROTOCOL."
  (case protocol
    (:chat-completions "/chat/completions")
    (:messages "/v1/messages")
    (:responses "/responses")
    (otherwise (copilot--error "Unsupported Copilot protocol: ~S." protocol))))

(defun copilot-http-headers (token &key (user-agent "cl-llm-provider-api"))
  "Return authenticated Copilot HTTP headers without exposing TOKEN elsewhere."
  (unless (non-empty-string-p token)
    (copilot--error "Copilot token must be a nonempty string."))
  (list (cons "Authorization" (format nil "Bearer ~A" token))
        (cons "Accept" "application/json")
        (cons "User-Agent" user-agent)
        (cons "Editor-Version" "vscode/1.107.0")
        (cons "Editor-Plugin-Version" "copilot-chat/0.35.0")
        (cons "Copilot-Integration-Id" "vscode-chat")))

(defun copilot--vision-p (value)
  "Return true when VALUE contains an image content block."
  (cond ((json-object-p value)
          (or (json-string-member-p (json-get value "type") '("image" "input_image" "image_url"))
              (loop for child being the hash-values of value thereis
                (copilot--vision-p child))))
         ((and (vectorp value) (not (stringp value))) (some #'copilot--vision-p value))
         ((consp value) (some #'copilot--vision-p value))
        (t nil)))

(defun copilot-stream-headers
    (token request &key protocol (initiator "user")
                       (user-agent "cl-llm-provider-api")
                       (anthropic-version "2023-06-01"))
  "Return authenticated SSE headers for a Copilot wire REQUEST."
  (unless (member protocol '(:chat-completions :messages :responses))
    (copilot--error "Unsupported Copilot protocol: ~S." protocol))
  (append (remove "Accept" (copilot-http-headers token :user-agent user-agent)
                  :key #'first :test #'string-equal)
          (list (cons "Accept" "text/event-stream")
                (cons "Content-Type" "application/json")
                (cons "X-Initiator" initiator)
                (cons "Openai-Intent" "conversation-edits"))
          (when (copilot--vision-p request)
            (list (cons "Copilot-Vision-Request" "true")))
          (when (eq protocol :messages)
            (list (cons "anthropic-version" anthropic-version)))))


(defun copilot-auto-session-request ()
  "Return the JSON object requesting an automatic-model session."
  (json-object "auto_mode" (json-object "model_hints" (vector "auto"))))

(defun copilot-vision-request-p (request)
  "Return exactly true or false when REQUEST contains an image content block."
  (not (null (copilot--vision-p request))))

(defun copilot-auto-session-model (session catalog &key (model-prefix "")
                                                       preferred-model vision-p)
  "Return the concrete model spec and token explicitly authorized by SESSION.
CATALOG is raw /models JSON, including models hidden from manual selection.
PREFERRED-MODEL is an unprefixed ID. Otherwise use the offered model order."
  (unless (and (json-object-p session) (stringp model-prefix)
               (or (null preferred-model) (stringp preferred-model)))
    (copilot--error "Copilot automatic-session response or namespace is invalid."))
  (let* ((selected (json-get session "selected_model"))
         (selected-p (nth-value 1 (gethash "selected_model" session)))
         (offered (json-get session "available_models"))
         (offered-p (nth-value 1 (gethash "available_models" session)))
         (token (json-get session "session_token"))
         (expires (json-get session "expires_at")))
    (when (and (nth-value 1 (gethash "expires_at" session))
               (not (typep expires '(integer 1))))
      (copilot--error "Copilot automatic-session expiry is invalid."))
    (unless (and (non-empty-string-p token)
                 (every (lambda (character) (<= 33 (char-code character) 126)) token))
      (copilot--error "Copilot automatic-session token is invalid."))
    (when (and offered-p
               (not (and (vectorp offered) (not (stringp offered))
                         (plusp (length offered)) (every #'non-empty-string-p offered))))
      (copilot--error "Copilot available models must be a nonempty array of IDs."))
    (when (and selected-p (not (non-empty-string-p selected)))
      (copilot--error "Copilot selected model must be a nonempty ID."))
    (unless (or selected-p offered-p)
      (copilot--error "Copilot automatic-session response authorizes no models."))
    (when (and selected-p offered-p (not (find selected offered :test #'string=)))
      (copilot--error "Copilot selected model is unavailable."))
    (let* ((known (remove-if-not
                   (lambda (entry)
                     (or (not vision-p)
                         (eq (copilot--field entry "capabilities" "supports" "vision") t)))
                   (copilot--catalog-entries catalog)))
           (candidate
             (labels ((find-model (id)
                        (find id known :key (lambda (entry) (json-get entry "id"))
                                       :test #'string=)))
               (if selected-p
                   (find-model selected)
                   (or (and preferred-model
                            (find preferred-model offered :test #'string=)
                            (find-model preferred-model))
                       (loop for id across offered
                             for entry = (find-model id)
                             when entry return entry))))))
      (unless candidate
        (copilot--error "Copilot automatic-session has no eligible known model."))
      (values (copilot--model-spec candidate model-prefix :automatic-p t) token))))
