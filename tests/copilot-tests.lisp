(in-package #:cl-llm-provider-api/tests)

(defun test-copilot-wire ()
  "Exercise Copilot catalog, endpoint, host, and header codecs."
  (let ((entry (cl-llm-provider-api::json-object "id" "claude-sonnet"
                            "supported_endpoints" (vector "/v1/messages"))))
    (check (eq (copilot-model-protocol entry) :messages)
           "Copilot Messages route was not selected")
    (check (eq (copilot-model-protocol (cl-llm-provider-api::json-object "id" "gpt-5")) :responses)
           "Copilot legacy Responses route was not selected")
    (check (eq (copilot-model-protocol (cl-llm-provider-api::json-object "id" "x"
                                                     "supported_endpoints"
                                                     (vector "/unknown")))
                :unsupported)
           "unknown Copilot route was not rejected")
    (let ((models (copilot-model-catalog
                   (cl-llm-provider-api::json-object "data"
                                (vector
                                 (cl-llm-provider-api::json-object "id" "m" "name" "Model"
                                              "model_picker_enabled" t
                                              "policy" (cl-llm-provider-api::json-object "state" "enabled")
                                              "capabilities"
                                              (cl-llm-provider-api::json-object "supports"
                                                           (cl-llm-provider-api::json-object "reasoning_effort" t))
                                              "supported_endpoints"
                                              (vector "/responses"))))
                   :model-prefix "copilot/")))
      (check (equal (first models)
                    '(:name "copilot/m" :protocol :responses :description "Model"
                      :reasoning-efforts ("low" "medium" "high")))
             "Copilot catalog metadata was not decoded")))
  (dolist (document (list (cl-llm-provider-api::json-object "data" "")
                         (cl-llm-provider-api::json-object "data" (vector 3))
                         (cl-llm-provider-api::json-object "data" (vector (cl-llm-provider-api::json-object "id" 3)))))
    (check (handler-case (progn (copilot-model-catalog document) nil)
             (provider-error () t))
           "Malformed Copilot catalogs must signal a typed error"))
  (dolist (endpoints (list "/responses" (vector 3) :json-false yason:false nil))
    (check (handler-case
               (progn (copilot-model-protocol
                       (cl-llm-provider-api::json-object "id" "m" "supported_endpoints" endpoints)) nil)
             (provider-error () t))
           "Malformed Copilot endpoint arrays must signal a typed error"))
  (let ((enabled nil))
    (flet ((entry (id &key (picker t) (state "unconfigured") (tools t) (endpoint "/responses"))
             (cl-llm-provider-api::json-object
              "id" id "model_picker_enabled" picker
              "supported_endpoints" (vector endpoint)
              "policy" (cl-llm-provider-api::json-object "state" state)
              "capabilities" (cl-llm-provider-api::json-object
                              "supports" (cl-llm-provider-api::json-object "tool_calls" tools)))))
      (let* ((document (cl-llm-provider-api::json-object
                        "data" (vector (entry "allowed") (entry "allowed")
                                       (entry "hidden" :picker :json-false)
                                       (entry "blocked" :state "disabled")
                                       (entry "no-tools" :tools :json-false)
                                       (entry "unsupported" :endpoint "/unknown"))))
             (models (copilot-model-catalog
                      document :enable-model-function
                      (lambda (id) (push id enabled) t))))
        (check (equal enabled '("allowed"))
               "Policy enablement must be limited to unique eligible tool models")
        (check (equal (mapcar (lambda (model) (getf model :name)) models) '("allowed"))
               "Disabled, hidden, non-tool, and unsupported models must be excluded")
        (check (null (copilot-model-catalog document))
               "Ordinary discovery must not enable an unconfigured model"))
      (let ((document (cl-llm-provider-api::json-object
                       "data" (vector (entry "personal" :picker :json-false :state "enabled")))))
        (check (= (length (copilot-model-catalog document :personal-account-p t)) 1)
               "Personal fallback must include an enabled tool model")
        (check (null (copilot-model-catalog document))
               "Business discovery must preserve picker restrictions"))))
  (check (string= (copilot-base-url "token" )
                  "https://api.individual.githubcopilot.com")
         "Copilot default base URL is wrong")
  (check (string= (copilot-base-url "proxy-ep=proxy.foo.githubcopilot.com")
                  "https://api.foo.githubcopilot.com")
         "Copilot proxy base URL is wrong")
  (check (handler-case (progn (copilot-base-url "proxy-ep=proxy.evil.example") nil)
           (provider-error () t))
         "untrusted Copilot proxy was accepted")
  (dolist (host '("proxy.individual.githubcopilot.com.evil.example"
                  "proxy.individual.githubcopilot.com/evil"
                  "proxy.individual.githubcopilot.com@evil.example"
                  "proxy..githubcopilot.com" "proxy.-bad.githubcopilot.com"))
    (check (handler-case (progn (copilot-base-url (concatenate 'string "proxy-ep=" host)) nil)
             (provider-error () t))
           "Malformed or untrusted Copilot proxy hosts must be rejected"))
  (check (equal (copilot-protocol-endpoint :messages) "/v1/messages")
         "Copilot endpoint was not selected")
  (check (handler-case (progn (copilot-protocol-endpoint :nope) nil)
           (provider-error () t))
         "unknown Copilot protocol was not typed")
  (let ((headers (copilot-stream-headers
                   "secret" (cl-llm-provider-api::json-object "messages"
                                         (vector (cl-llm-provider-api::json-object "type" "image_url")
                                                 (cl-llm-provider-api::json-object "type" 42)))
                  :protocol :messages)))
    (check (and (equal (response-header headers "Copilot-Vision-Request") "true")
                (equal (response-header headers "anthropic-version") "2023-06-01")
                (equal (response-header headers "Accept") "text/event-stream"))
           "Copilot stream headers are incomplete")
    (check (null (response-header
                  (copilot-stream-headers "secret"
                                          (cl-llm-provider-api::json-object "max_tokens" 3)
                                          :protocol :responses)
                  "anthropic-version"))
           "Anthropic version leaked into Responses headers")))


(defun test-copilot-auto-wire ()
  "Exercise Auto entitlement decoding, concrete protocols, capability filtering, and secrecy."
  (flet ((object (&rest fields)
           (apply #'cl-llm-provider-api::json-object fields)))
    (let* ((request (copilot-auto-session-request))
           (chat (object "id" "chat" "model_picker_enabled" :json-false
                         "supported_endpoints" (vector "/chat/completions")
                         "capabilities" (object "limits" (object "max_prompt_tokens" 4096))))
           (responses (object "id" "responses" "model_picker_enabled" :json-false
                              "supported_endpoints" (vector "/responses")
                              "capabilities" (object "limits" (object "max_prompt_tokens" 8192)
                                                     "supports" (object "vision" t))))
           (messages (object "id" "messages" "model_picker_enabled" :json-false
                             "supported_endpoints" (vector "/v1/messages")))
           (no-tools (object "id" "no-tools" "capabilities"
                             (object "supports" (object "tool_calls" :json-false))))
           (unsupported (object "id" "unsupported" "supported_endpoints" (vector "/unknown")))
           (document (object "data" (vector chat responses messages no-tools unsupported)))
           (token "session-secret"))
      (check (and (hash-table-p request)
                  (equalp (gethash "model_hints" (gethash "auto_mode" request)) (vector "auto")))
             "Auto must request an authorized session with an object body")
      (let ((models (copilot-model-catalog document :model-prefix "copilot/" :include-auto-p t)))
        (check (equal (mapcar (lambda (model) (getf model :name)) models) '("copilot/auto"))
               "Hidden concrete models must be available only through Auto")
        (check (and (eq (getf (first models) :protocol) :auto)
                    (= (getf (first models) :context-window) 4096))
               "Auto must use the conservative known context limit"))
      (check (null (copilot-model-catalog (object "data" #()) :include-auto-p t))
             "Empty accounts must not advertise Auto")
      (check (null (copilot-model-catalog (object "data" (vector no-tools unsupported)) :include-auto-p t))
             "Non-tool and unsupported accounts must not advertise Auto")
      (let ((models (copilot-model-catalog (object "data" (vector messages)) :include-auto-p t)))
        (check (and models (null (getf (first models) :context-window)))
               "Models without limits must not fail Auto discovery"))
      (dolist (spec '(("chat" :chat-completions) ("responses" :responses) ("messages" :messages)))
        (multiple-value-bind (model session-token)
            (copilot-auto-session-model (object "selected_model" (first spec) "session_token" token)
                                        document :model-prefix "copilot/")
          (check (and (equal (getf model :name) (concatenate 'string "copilot/" (first spec)))
                      (eq (getf model :protocol) (second spec))
                      (equal session-token token))
                 "Explicit Auto selection must preserve the concrete protocol")))
      (dolist (spec '((nil nil "messages") ("chat" nil "chat") ("unavailable" nil "messages")
                      ("chat" t "responses")))
        (let* ((session (object "available_models" (vector "unknown" "messages" "responses" "chat")
                                "session_token" token "expires_at" 10))
               (model (copilot-auto-session-model session document
                                                 :preferred-model (first spec) :vision-p (second spec))))
          (check (equal (getf model :name) (third spec))
                 "Auto must select only offered capable models, honoring affinity and server order")))
      (dolist (session
               (list (object "session_token" token)
                     (object "session_token" token "available_models" #())
                     (object "session_token" token "available_models" "chat")
                     (object "session_token" token "available_models" (vector 3))
                     (object "session_token" token "available_models" (vector ""))
                     (object "session_token" token "available_models" nil)
                     (object "session_token" token "available_models" (vector "unknown"))
                     (object "session_token" token "selected_model" "")
                     (object "session_token" token "selected_model" nil)
                     (object "session_token" token "selected_model" 3)
                     (object "session_token" token "selected_model" "unknown")
                     (object "session_token" token "selected_model" "no-tools")
                     (object "session_token" token "selected_model" "unsupported")
                     (object "session_token" token "selected_model" "auto")
                     (object "session_token" token "selected_model" "chat" "available_models" (vector "responses"))
                     (object "selected_model" "chat")
                     (object "session_token" "" "selected_model" "chat")
                     (object "session_token" (format nil "session-secret~%injection") "selected_model" "chat")
                     (object "session_token" token "selected_model" "chat" "expires_at" 0)
                     (object "session_token" token "selected_model" "chat" "expires_at" nil)
                     (object "session_token" token "selected_model" "chat" "expires_at" :json-false)))
        (check (handler-case (progn (copilot-auto-session-model session document) nil)
                 (provider-error (condition) (not (search token (princ-to-string condition)))))
               "Malformed or unauthorized Auto sessions must fail without revealing tokens"))
      (check (handler-case
                 (progn (copilot-auto-session-model (object "selected_model" "chat" "session_token" token)
                                                    document :vision-p t) nil)
               (provider-error () t))
             "An explicitly selected non-vision model must not receive images")
      (dolist (catalog (list (object "data" "") (object "data" (vector 3))
                             (object "data" (vector (object "id" "")))
                             (object "data" (vector (object "id" "bad" "supported_endpoints" nil)))))
        (check (handler-case
                   (progn (copilot-auto-session-model
                           (object "selected_model" "chat" "session_token" token) catalog) nil)
                 (provider-error () t))
               "Auto must reject malformed concrete catalogs with typed errors"))
      (check (eq (copilot-vision-request-p
                  (object "nested" (vector (object "type" "input_image")))) t)
             "Nested image requests must enable vision filtering")
      (check (null (copilot-vision-request-p (object "messages" (vector (object "type" "text")))))
             "Text requests must not require vision"))))
