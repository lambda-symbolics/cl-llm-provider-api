(in-package #:cl-llm-provider-api)

(define-condition gemini-code-assist-setup-error (provider-protocol-error)
  ((stage :initarg :stage :reader gemini-code-assist-setup-error-stage
          :documentation "The setup RPC stage at which the failure occurred."))
  (:default-initargs :status nil :request-id nil :response nil)
  (:documentation "A malformed or unsuccessful Gemini Code Assist setup response."))

(defun gemini-code-assist--decode-setup-response (body stage)
  "Decode a setup response object, reporting only bounded, redacted diagnostics."
  (flet ((invalid (message)
           (error 'gemini-code-assist-setup-error :stage stage :message message
                  :response (bounded-string (provider--sanitize-wire-string body)
                                            :limit 2000))))
    (let ((response (handler-case (json-decode body)
                      (provider-api-error ()
                        (invalid "Gemini Code Assist returned invalid JSON.")))))
      (unless (json-object-p response)
        (invalid "Gemini Code Assist returned a non-object JSON response."))
      (let ((failure (json-get response "error")))
        (when failure
          (let ((message (and (json-object-p failure) (json-get failure "message")))
                (code    (and (json-object-p failure) (json-get failure "code"))))
            (error 'gemini-code-assist-setup-error
                   :stage   stage
                   :status  (and (integerp code) code)
                   :message (if (non-empty-string-p message)
                                (bounded-string (provider--sanitize-wire-string message)
                                                :limit 2000)
                                "Gemini Code Assist returned an RPC error.")))))
      response)))

(defun gemini-code-assist--signal-terminal-setup-failure (stage condition)
  "Preserve normalized diagnostics without letting an outer retry replay onboarding."
  (error 'gemini-code-assist-setup-error
         :stage      stage
         :message    (provider-api-error-message condition)
         :status     (provider-error-status condition)
         :request-id (provider-error-request-id condition)
         :response   (provider-error-response condition)))

(defun gemini-code-assist--setup-request (provider transport sleep-function &key
                                          stage method target payload)
  "Decode one setup RPC, retrying transient failures only for read-only RPCs."
  (loop for attempt from 1
        do (handler-case
               (return
                 (multiple-value-bind (body status headers)
                     (funcall transport method target payload)
                   (if (<= 200 status 299)
                       (gemini-code-assist--decode-setup-response body stage)
                       (provider--signal-http-status-failure
                        provider status :headers headers :raw-body body))))
             (provider-retryable-error (condition)
               (cond ((eq stage ':onboard)
                      (gemini-code-assist--signal-terminal-setup-failure stage condition))
                     ((< attempt 4)
                      (funcall sleep-function 1))
                     ((eq stage ':operation)
                      (gemini-code-assist--signal-terminal-setup-failure stage condition))
                     (t
                      (error condition))))
             (provider-unauthorized (condition)
               ;; A rejected POST can be replayed after refresh; a rejected poll cannot.
               (if (eq stage ':operation)
                   (gemini-code-assist--signal-terminal-setup-failure stage condition)
                   (error condition))))))

(defun gemini-code-assist--setup-metadata (project)
  "Return Code Assist client metadata for PROJECT."
  (let ((metadata (json-object "ideType" "IDE_UNSPECIFIED"
                               "platform" "PLATFORM_UNSPECIFIED"
                               "pluginType" "GEMINI")))
    (when project
      (setf (gethash "duetProject" metadata) project))
    metadata))

(defun gemini-code-assist--default-setup-tier (load-response)
  "Return the default allowed tier or the legacy fallback."
  (let ((tiers (json-get load-response "allowedTiers")))
    (or (and (vectorp tiers)
             (loop for tier across tiers
                   when (and (json-object-p tier) (json-get tier "isDefault"))
                     return tier))
        (json-object "id" "legacy-tier"))))

(defun gemini-code-assist--poll-setup (operation poll sleep-function &key maximum-polls)
  "Return the completed operation after at most MAXIMUM-POLLS successful polls."
  (loop for polls from 0
        for name = (json-get operation "name")
        do (cond ((json-get operation "done")
                  (return operation))
                 ((not (non-empty-string-p name))
                  (error 'gemini-code-assist-setup-error
                         :stage   ':operation
                         :message "Gemini Code Assist returned an unfinished operation without a name."))
                 ((= polls maximum-polls)
                  (error 'gemini-code-assist-setup-error
                         :stage   ':operation
                         :message "Gemini Code Assist onboarding did not finish within the polling limit.")))
           (funcall sleep-function 5)
           (setf operation (funcall poll name))))

(defun gemini-code-assist-run-setup (provider transport &key project
                                     (sleep-function #'sleep) (maximum-polls 60))
  "Resolve the setup project and tier without persisting caller state.

TRANSPORT receives (METHOD TARGET PAYLOAD) and returns a string body, integer
HTTP status, and response headers, or signals a normalized provider failure.
METHOD is :POST for an RPC method name and :GET for an operation name (NIL
payload). The caller owns credentials and their diagnostic-redaction scope.
Return four values: effective project, tier, missing-project stage, and the
load response. The last two are NIL when a project was resolved. Onboarding
is submitted at most once per call; load and polls retry transient failures."
  (check-type maximum-polls (integer 0))
  (flet ((post (stage target payload)
           (gemini-code-assist--setup-request provider transport sleep-function
                                             :stage stage :method ':post
                                             :target target :payload payload))
         (poll (name)
           (gemini-code-assist--setup-request provider transport sleep-function
                                             :stage ':operation :method ':get
                                             :target name)))
    (let* ((load-response
             (post ':load "loadCodeAssist"
                   (json-object "cloudaicompanionProject" project
                                "metadata" (gemini-code-assist--setup-metadata project))))
           (current (json-get load-response "currentTier")))
      (if (json-object-p current)
          (let ((paid      (json-get load-response "paidTier"))
                (effective (or (json-get load-response "cloudaicompanionProject") project)))
            (values effective
                    (or (and (json-object-p paid) (json-get paid "id"))
                        (json-get current "id") "standard-tier")
                    (unless effective ':load) (unless effective load-response)))
          (let* ((tier      (gemini-code-assist--default-setup-tier load-response))
                 (tier-id   (or (json-get tier "id") "standard-tier"))
                 (requested (unless (string= tier-id "free-tier") project))
                 (operation (post ':onboard "onboardUser"
                                  (json-object "tierId" tier-id
                                               "cloudaicompanionProject" requested
                                               "metadata" (gemini-code-assist--setup-metadata
                                                           requested))))
                 (completed (gemini-code-assist--poll-setup
                             operation #'poll sleep-function :maximum-polls maximum-polls))
                 (response  (json-get completed "response"))
                 (companion (and (json-object-p response)
                                 (json-get response "cloudaicompanionProject")))
                 (effective (or (and (json-object-p companion) (json-get companion "id"))
                                project)))
            (values effective tier-id (unless effective ':onboard)
                    (unless effective load-response)))))))
