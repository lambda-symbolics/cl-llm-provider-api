(in-package #:cl-llm-provider-api)

;;;; -- Vertex Routing and Local ADC Tests --

(defun vertex-test--routing ()
  "Exercise native model routing, body adaptation, and quota headers."
  (dolist (location '("global" "us-east5"))
    (let* ((gemini      (make-instance 'vertex-gemini-provider :project "test-project" :location location))
           (claude      (make-instance 'vertex-anthropic-provider :project "test-project" :location location))
           (host        (if (string= location "global") "aiplatform.googleapis.com"
                            "us-east5-aiplatform.googleapis.com"))
           (credentials (make-instance 'google-cloud-credentials
                                       :access-token "test-access" :quota-project "quota-project")))
      (dolist (entry (list (list gemini "gemini-3.5-flash" "google" "streamGenerateContent?alt=sse")
                          (list claude "claude-haiku-4-5@20251001" "anthropic" "streamRawPredict")))
        (destructuring-bind (provider model publisher operation) entry
          (let* ((request (provider-request-object
                           provider (make-instance 'wire-request :model model
                                                   :items               (list (json-object "type" "message"
                                                                                           "role" "user" "content" "hi")))
                           #()))
                 (body    (vertex-request-body provider request)))
            (test-assert (string= (vertex-request-url provider model)
                                  (format nil "https://~A/v1/projects/test-project/locations/~A/publishers/~A/models/~A:~A"
                                          host location publisher model operation))
                         "Vertex routes the protocol to its native streaming operation")
            (test-assert (and (string= (json-get request "model") model)
                              (not (nth-value 1 (gethash "model" body))))
                         "routing preserves the logical request but omits model from the wire body")
            (when (eq provider claude)
              (test-assert (string= (json-get body "anthropic_version") "vertex-2023-10-16")
                           "Vertex carries Anthropic's version in the body"))
            (let ((headers (vertex-request-headers provider credentials)))
              (test-assert (and (equal (response-header headers "Authorization") "Bearer test-access")
                                (equal (response-header headers "x-goog-user-project") "quota-project")
                                (not (response-header headers "x-api-key"))
                                (not (response-header headers "anthropic-version")))
                           "Vertex uses OAuth and the ADC quota project")))))))
  (dolist (arguments '((:project "project" :location "evil.example/")
                       (:project "project?key=secret") (:project "../escape")))
    (test-assert (handler-case (progn (apply #'make-instance 'vertex-gemini-provider arguments) nil)
                   (provider-api-error () t))
                 "malformed routing cannot redirect bearer credentials"))
  (let ((provider (make-instance 'vertex-gemini-provider :project "project")))
    (test-assert (handler-case (progn (vertex-request-url provider "model?key=secret") nil)
                   (provider-api-error () t))
                 "models cannot inject URL query parameters")))

(defun vertex-test--adc ()
  "Refresh local user ADC without persisting or exposing credential material."
  (let ((environment '(("GOOGLE_APPLICATION_CREDENTIALS" . "/explicit/adc.json")
                       ("CLOUDSDK_CONFIG" . "/config/"))))
    (flet ((getenv (name) (rest (assoc name environment :test #'string=))))
      (test-assert (equal (google-cloud-default-credentials-path :getenv #'getenv)
                          #P"/explicit/adc.json") "explicit ADC has precedence")
      (setf environment (rest environment))
      (test-assert (equal (google-cloud-default-credentials-path :getenv #'getenv)
                          #P"/config/application_default_credentials.json") "Cloud SDK config is honored")
      (setf environment '(("APPDATA" . "/appdata/")))
      (test-assert (equal (google-cloud-default-credentials-path :getenv #'getenv :windows-p t)
                          #P"/appdata/gcloud/application_default_credentials.json") "Windows ADC uses APPDATA")))
  (uiop:with-temporary-file (:pathname path :stream stream :direction ':output)
    (let* ((original (json-encode (json-object "type" "authorized_user" "client_id" "test-client"
                                              "client_secret" "test-client-secret" "refresh_token" "test-refresh"
                                              "quota_project_id" "quota-project"
                                              "token_uri" "https://untrusted.invalid/token")))
           (calls    0))
      (write-string original stream)
      (finish-output stream)
      (flet ((request (url &key headers content)
               (declare (ignore headers))
               (incf calls)
               (test-assert (and (string= url "https://oauth2.googleapis.com/token")
                                 (equal (rest (assoc "grant_type" content :test #'string=)) "refresh_token")
                                 (equal (rest (assoc "refresh_token" content :test #'string=)) "test-refresh"))
                            "refresh uses Google's fixed endpoint and the ADC grant")
               (values (json-encode (json-object "access_token" (format nil "access-~D" calls)
                                                  "expires_in" 3600 "token_type" "Bearer")) 200)))
        (let ((first  (google-cloud-user-credentials path #'request))
              (second (google-cloud-user-credentials path #'request)))
          (test-assert (and (= calls 2)
                            (equal (cl-rfc8628:oauth-credentials-access-token first) "access-1")
                            (equal (cl-rfc8628:oauth-credentials-access-token second) "access-2")
                            (null (cl-rfc8628:oauth-credentials-refresh-token second))
                            (equal (google-cloud-credentials-quota-project second) "quota-project"))
                       "each use refreshes without retaining the user's refresh token")))
      (test-assert (string= original (uiop:read-file-string path)) "ADC is read-only")
      (dolist (mode '(:rejected :exception :malformed :header-injection))
        (let ((condition
                (handler-case
                    (google-cloud-user-credentials
                     path (lambda (&rest arguments)
                            (declare (ignore arguments))
                            (case mode
                              (:rejected
                               (values "test-refresh test-client-secret" 400))
                              (:exception
                               (error "test-refresh test-client-secret"))
                              (:malformed
                               (values "not JSON test-refresh" 200))
                              (:header-injection
                               (values (json-encode
                                        (json-object "access_token" (format nil "access~%injected")
                                                     "expires_in" 3600 "token_type" "Bearer")) 200)))))
                  (google-cloud-authentication-error (condition) condition))))
          (test-assert (and (typep condition 'google-cloud-authentication-error)
                            (not (search "test-refresh" (princ-to-string condition)))
                            (not (search "test-client-secret" (princ-to-string condition))))
                       "token failures and malformed success bodies do not expose credentials"))))))

(defun vertex-test--invalid-adc ()
  "Reject unsupported and malformed files before any token exchange."
  (dolist (document '("{}" "not JSON" "{\"type\":\"service_account\",\"private_key\":\"test-private\"}"
                      "{\"type\":\"authorized_user\",\"refresh_token\":\"test-refresh\"}"))
    (uiop:with-temporary-file (:pathname path :stream stream :direction ':output)
      (write-string document stream)
      (finish-output stream)
      (let* ((called    nil)
             (condition (handler-case
                            (google-cloud-user-credentials
                             path (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (setf called t)))
                          (google-cloud-authentication-error (condition) condition))))
        (test-assert (and (not called) (typep condition 'google-cloud-authentication-error)
                          (eq (google-cloud-authentication-error-stage condition) ':credentials)
                          (not (search "test-private" (princ-to-string condition)))
                          (not (search "test-refresh" (princ-to-string condition))))
                     "invalid local ADC fails before sending secrets")))))

(defun run-vertex-tests ()
  "Run Vertex routing and local ADC contracts."
  (vertex-test--routing)
  (vertex-test--adc)
  (vertex-test--invalid-adc))
