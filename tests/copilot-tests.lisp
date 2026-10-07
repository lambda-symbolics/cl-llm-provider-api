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
  (dolist (endpoints (list "/responses" (vector 3)))
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
