(in-package #:cl-llm-provider-api)

;;;; -- Google Cloud Local User Credentials --

(define-condition google-cloud-authentication-error (provider-api-error)
  ((stage :initarg :stage :reader google-cloud-authentication-error-stage
          :documentation "The credential discovery, file, or token exchange stage.")
   (status :initarg :status :initform nil :reader google-cloud-authentication-error-status
           :documentation "The token endpoint's HTTP status, when available."))
  (:documentation "A Google Cloud credential failure containing no credential material."))

(defclass google-cloud-credentials (cl-rfc8628:oauth-credentials)
  ((quota-project :initarg :quota-project :initform nil
                  :reader google-cloud-credentials-quota-project
                  :documentation "The optional billing and quota project from ADC."))
  (:documentation "Request-scoped Google Cloud access credentials."))

(defun google-cloud--authentication-failure (stage &optional status)
  "Signal a credential failure without including files, tokens, or response bodies."
  (error 'google-cloud-authentication-error
         :stage stage :status status
         :message (case stage
                    (:discovery "No local Google Cloud ADC file was found. Run gcloud auth application-default login.")
                    (:credentials "Expected local user ADC credentials. Run gcloud auth application-default login.")
                    (otherwise "Google Cloud token refresh failed. Run gcloud auth application-default login."))))

(defun google-cloud-default-credentials-path
    (&key (getenv #'uiop:getenv) (home (user-homedir-pathname))
          (windows-p (uiop:os-windows-p)))
  "Find local ADC, honoring GOOGLE_APPLICATION_CREDENTIALS and CLOUDSDK_CONFIG.
Only local authorized_user credentials are supported by GOOGLE-CLOUD-USER-CREDENTIALS."
  (flet ((environment (name)
           (let ((value (funcall getenv name)))
             (and (non-empty-string-p value) value))))
    (let ((explicit (environment "GOOGLE_APPLICATION_CREDENTIALS"))
          (config (environment "CLOUDSDK_CONFIG")))
      (cond
        (explicit (pathname explicit))
        (config (merge-pathnames "application_default_credentials.json"
                                 (uiop:ensure-directory-pathname config)))
        (windows-p
         (let ((appdata (environment "APPDATA")))
           (unless appdata (google-cloud--authentication-failure ':discovery))
           (merge-pathnames "gcloud/application_default_credentials.json"
                            (uiop:ensure-directory-pathname appdata))))
        (t (merge-pathnames ".config/gcloud/application_default_credentials.json" home))))))

(defun google-cloud--read-user-credentials (pathname)
  "Read a bounded local ADC file and reject unsupported credential types."
  (handler-case
      (let ((document
              (with-open-file (stream pathname :direction ':input :external-format ':utf-8)
                (let* ((buffer (make-string (1+ (* 1024 1024))))
                       (length (read-sequence buffer stream)))
                  (when (> length (* 1024 1024))
                    (google-cloud--authentication-failure ':credentials))
                  (json-decode (subseq buffer 0 length))))))
        (unless (and (json-object-p document)
                     (json-string= (json-get document "type") "authorized_user")
                     (every (lambda (key) (non-empty-string-p (json-get document key)))
                            '("client_id" "client_secret" "refresh_token")))
          (google-cloud--authentication-failure ':credentials))
        document)
    (google-cloud-authentication-error (condition) (error condition))
    (error () (google-cloud--authentication-failure ':credentials))))

(defun google-cloud--header-value-p (value)
  "Return true for a nonempty ASCII header value without whitespace or controls."
  (and (non-empty-string-p value)
       (every (lambda (character) (<= 33 (char-code character) 126)) value)
       t))

(defun google-cloud-user-credentials (pathname request-function)
  "Exchange local user ADC at PATHNAME for fresh, request-scoped credentials.
REQUEST-FUNCTION accepts URL and :HEADERS and :CONTENT, where CONTENT is a form
parameter alist. It returns a JSON string and HTTP status. No credential file
is written and no tokens are cached. The caller owns the returned credentials."
  (let* ((document (google-cloud--read-user-credentials pathname))
         (quota-project (json-get document "quota_project_id")))
    (handler-case
        (multiple-value-bind (body status)
            (funcall request-function "https://oauth2.googleapis.com/token"
                     :headers '(("Content-Type" . "application/x-www-form-urlencoded"))
                     :content (list (cons "grant_type" "refresh_token")
                                    (cons "client_id" (json-get document "client_id"))
                                    (cons "client_secret" (json-get document "client_secret"))
                                    (cons "refresh_token" (json-get document "refresh_token"))))
          (unless (and (integerp status) (<= 200 status 299))
            (google-cloud--authentication-failure ':token status))
          (let* ((response (json-decode body))
                 (token (json-get response "access_token"))
                 (expires-in (json-get response "expires_in")))
            (unless (and (google-cloud--header-value-p token)
                         (string-equal (or (json-get response "token_type") "") "Bearer")
                         (typep expires-in '(integer 1))
                         (or (null quota-project) (google-cloud--header-value-p quota-project)))
              (google-cloud--authentication-failure ':token status))
            (make-instance 'google-cloud-credentials
                           :access-token token :refresh-token nil :id-token nil
                           :account-id nil :expires-at (+ (get-universal-time) expires-in)
                           :source-path pathname :quota-project quota-project)))
      (google-cloud-authentication-error (condition) (error condition))
      (error () (google-cloud--authentication-failure ':token)))))
