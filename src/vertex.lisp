(in-package #:cl-llm-provider-api)

;;;; -- Google Vertex AI Transports --

(defclass vertex-provider-mixin ()
  ((project :initarg :project :reader vertex-provider-project
            :documentation "The Google Cloud resource project ID or number.")
   (location :initarg :location :initform "global" :reader vertex-provider-location
             :documentation "The Vertex location, including global.")
   (quota-project :initarg :quota-project :initform nil :reader vertex-provider-quota-project
                  :documentation "An optional quota project overriding the ADC value."))
  (:documentation "Project and location routing shared by Vertex model protocols."))

(defclass vertex-gemini-provider (vertex-provider-mixin gemini-generate-content-provider)
  ()
  (:documentation "Gemini GenerateContent through Google Vertex AI."))

(defclass vertex-anthropic-provider (vertex-provider-mixin anthropic-messages-provider)
  ()
  (:documentation "Claude Messages through Google Vertex AI."))

(defun vertex--resource-component-p (value)
  "Accept one resource path component, never a path, query, or URL."
  (and (non-empty-string-p value)
       (not (member value '("." "..") :test #'string=))
       (every (lambda (character)
                (or (find character "-_.@:")
                    (find character "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")))
              value)
       t))

(defmethod initialize-instance :after ((provider vertex-provider-mixin) &key)
  "Reject malformed project and location values before constructing a Vertex URL."
  (unless (and (slot-boundp provider 'project)
               (vertex--resource-component-p (vertex-provider-project provider))
               (non-empty-string-p (vertex-provider-location provider))
               (every (lambda (character)
                        (find character "abcdefghijklmnopqrstuvwxyz0123456789-"))
                      (vertex-provider-location provider)))
    (error 'provider-api-error :message "Vertex requires a project and a valid location.")))

(defgeneric vertex-request-url (provider model)
  (:documentation "Return the streaming Vertex URL for MODEL without credentials."))

(defun vertex--model-url (provider model &key publisher operation)
  "Build one Vertex streaming URL from validated resource components."
  (unless (vertex--resource-component-p model)
    (error 'provider-api-error :message "Invalid Vertex model identifier."))
  (let ((location (vertex-provider-location provider)))
    (format nil "https://~A/v1/projects/~A/locations/~A/publishers/~A/models/~A:~A"
            (if (string= location "global")
                "aiplatform.googleapis.com"
                (format nil "~A-aiplatform.googleapis.com" location))
            (vertex-provider-project provider) location publisher model operation)))

(defmethod vertex-request-url ((provider vertex-gemini-provider) model)
  "Route Gemini to the native GenerateContent SSE endpoint."
  (concatenate 'string (vertex--model-url provider model
                                          :publisher "google"
                                          :operation "streamGenerateContent")
               "?alt=sse"))

(defmethod vertex-request-url ((provider vertex-anthropic-provider) model)
  "Route Claude to the Messages-compatible raw prediction stream."
  (vertex--model-url provider model
                     :publisher "anthropic"
                     :operation "streamRawPredict"))

(defmethod provider-request-object :around
    ((provider vertex-gemini-provider) (projection wire-request) (tools vector)
     &key goal-context compaction-p)
  "Keep the selected model with the request until transport routing."
  (declare (ignore goal-context compaction-p))
  (let ((request (call-next-method)))
    (setf (gethash "model" request) (wire-request-model projection))
    request))

(defgeneric vertex-request-body (provider request)
  (:documentation "Copy REQUEST into its Vertex body, excluding the routing model."))

(defmethod vertex-request-body ((provider vertex-provider-mixin) (request hash-table))
  "Remove the URL-only model without changing the request used for retries."
  (declare (ignore provider))
  (let ((body (json-object-copy request)))
    (remhash "model" body)
    body))

(defmethod vertex-request-body :around
    ((provider vertex-anthropic-provider) (request hash-table))
  "Send Vertex's Anthropic API version in the body rather than a header."
  (let ((body (call-next-method)))
    (setf (gethash "anthropic_version" body) "vertex-2023-10-16")
    body))

(defun vertex-request-headers (provider credentials)
  "Return bearer and optional quota headers for request-scoped CREDENTIALS."
  (let ((token (cl-rfc8628:oauth-credentials-access-token credentials))
        (quota (or (vertex-provider-quota-project provider)
                   (and (typep credentials 'google-cloud-credentials)
                        (google-cloud-credentials-quota-project credentials)))))
    (unless (and (google-cloud--header-value-p token)
                 (or (null quota) (google-cloud--header-value-p quota)))
      (error 'provider-api-error :message "Invalid Vertex authorization headers."))
    (append (list (cons "Authorization" (concatenate 'string "Bearer " token))
                  (cons "Content-Type" "application/json")
                  (cons "Accept" "text/event-stream"))
            (when quota (list (cons "x-goog-user-project" quota))))))
