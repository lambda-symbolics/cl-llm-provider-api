(in-package #:cl-llm-provider-api)

(defclass gemini-code-assist-test-provider (gemini-generate-content-provider) ()
  (:documentation "A test provider with one extra retryable HTTP status."))

(defmethod provider-retryable-status-p ((provider gemini-code-assist-test-provider)
                                        (status integer) headers)
  "Treat the Code Assist 599 response as transient for setup fixtures."
  (or (= status 599) (call-next-method)))

(define-condition gemini-code-assist-test-cancelled (serious-condition) ()
  (:documentation "Cancellation raised by a scripted setup transport."))

(defun gemini-code-assist-test--script (steps &key project (maximum-polls 60))
  "Run setup against STEPS of (method target status body &optional payload-check).
Return its captured values or condition plus requested sleeps."
  (let ((provider (make-instance 'gemini-code-assist-test-provider))
        (sleeps   nil))
    (flet ((transport (method target payload)
             (assert steps () "Unexpected setup request: ~S ~A" method target)
             (destructuring-bind (verb name status body &optional check) (pop steps)
               (assert (eq method verb))
               (assert (string= target name))
               (when check (funcall check payload))
               (if (functionp body)
                   (funcall body provider)
                   (values body status nil)))))
      (let ((result (handler-case
                        (multiple-value-list
                         (gemini-code-assist-run-setup
                          provider #'transport :project project :maximum-polls maximum-polls
                          :sleep-function (lambda (seconds) (push seconds sleeps))))
                      (provider-error (condition) condition))))
        (assert (null steps) () "Setup did not consume the expected requests.")
        (values result (nreverse sleeps))))))

(defun gemini-code-assist-test--load ()
  "Retry both returned and pre-normalized errors and prefer the managed project and paid tier."
  (multiple-value-bind (result sleeps)
      (gemini-code-assist-test--script
       (list (list :post "loadCodeAssist" 599 "transient")
             (list :post "loadCodeAssist" nil
                   (lambda (provider)
                     (provider--signal-http-status-failure provider 599 :raw-body "transient")))
             (list :post "loadCodeAssist" 200
                   "{\"currentTier\":{\"id\":\"free-tier\"},\"paidTier\":{\"id\":\"paid-tier\"},\"cloudaicompanionProject\":\"managed\"}"
                   (lambda (payload)
                     (assert (string= (json-get payload "cloudaicompanionProject") "configured"))
                     (assert (string= (json-get (json-get payload "metadata") "duetProject")
                                      "configured")))))
       :project "configured")
    (assert (equal result '("managed" "paid-tier" nil nil)))
    (assert (equal sleeps '(1 1))))
  (multiple-value-bind (failure sleeps)
      (gemini-code-assist-test--script
       (loop repeat 4 collect (list :post "loadCodeAssist" 599 "transient")))
    (assert (typep failure 'provider-retryable-error))
    (assert (= 599 (provider-error-status failure)))
    (assert (equal sleeps '(1 1 1)))))

(defun gemini-code-assist-test--onboard ()
  "Route free-tier onboarding and retry only polling, never the write."
  (multiple-value-bind (result sleeps)
      (gemini-code-assist-test--script
       (list (list :post "loadCodeAssist" 200
                   "{\"allowedTiers\":[{\"id\":\"legacy-tier\",\"isDefault\":false},{\"id\":\"free-tier\",\"isDefault\":true}]}")
             (list :post "onboardUser" 200 "{\"done\":false,\"name\":\"operations/setup\"}"
                   (lambda (payload)
                     (assert (string= (json-get payload "tierId") "free-tier"))
                     (assert (and (null (json-get payload "cloudaicompanionProject"))
                                  (not (nth-value 1 (json-get (json-get payload "metadata")
                                                              "duetProject")))))))
             (list :get "operations/setup" 503 "transient")
             (list :get "operations/setup" 200
                   "{\"done\":true,\"response\":{\"cloudaicompanionProject\":{\"id\":\"granted\"}}}"
                   (lambda (payload) (assert (null payload)))))
       :project "configured")
    (assert (equal result '("granted" "free-tier" nil nil)))
    (assert (equal sleeps '(5 1))))
  (multiple-value-bind (failure sleeps)
      (gemini-code-assist-test--script
       (list (list :post "loadCodeAssist" 200 "{}")
             (list :post "onboardUser" 599 "ambiguous write failure"
                   (lambda (payload)
                     (assert (string= (json-get payload "tierId") "legacy-tier"))
                     (assert (string= (json-get payload "cloudaicompanionProject") "configured")))))
       :project "configured")
    (assert (eq (gemini-code-assist-setup-error-stage failure) :onboard))
    (assert (= (provider-error-status failure) 599))
    (assert (null sleeps)))
  (multiple-value-bind (failure sleeps)
      (gemini-code-assist-test--script
       '((:post "loadCodeAssist" 200 "{}")
         (:post "onboardUser" 200 "{\"name\":\"operations/setup\"}")
         (:get "operations/setup" 401 "unauthorized")))
    (assert (eq (gemini-code-assist-setup-error-stage failure) :operation))
    (assert (= (provider-error-status failure) 401))
    (assert (equal sleeps '(5))))
  (multiple-value-bind (failure sleeps)
      (gemini-code-assist-test--script
       '((:post "loadCodeAssist" 200 "{}")
         (:post "onboardUser" 200 "{\"name\":\"operations/setup\"}")
         (:get "operations/setup" 200 "{\"name\":\"operations/setup\"}"))
       :maximum-polls 1)
    (assert (eq (gemini-code-assist-setup-error-stage failure) :operation))
    (assert (equal sleeps '(5)))))

(defun gemini-code-assist-test--setup-errors ()
  "Keep project-required policy outside protocol errors and preserve cancellation and redaction."
  (destructuring-bind (project tier stage load-response)
      (gemini-code-assist-test--script
       '((:post "loadCodeAssist" 200 "{\"currentTier\":{},\"ineligibleTiers\":[{}]}")))
    (assert (null project))
    (assert (string= tier "standard-tier"))
    (assert (eq stage :load))
    (assert (= 1 (length (json-get load-response "ineligibleTiers")))))
  (let ((*provider-active-credential-values* '("fixture-secret")))
    (dolist (body (list (concatenate 'string "bad fixture-secret " (make-string 2200))
                        "[\"fixture-secret\"]"))
      (multiple-value-bind (failure sleeps)
          (gemini-code-assist-test--script (list (list :post "loadCodeAssist" 200 body)))
        (assert (eq (gemini-code-assist-setup-error-stage failure) :load))
        (assert (not (search "fixture-secret" (provider-error-response failure))))
        (assert (<= (length (provider-error-response failure)) 2000))
        (assert (null sleeps))))
    (let ((failure (gemini-code-assist-test--script
                    '((:post "loadCodeAssist" 200 "{}")
                      (:post "onboardUser" 200 "{\"name\":\"operations/setup\"}")
                      (:get "operations/setup" 200
                       "{\"done\":true,\"error\":{\"code\":7,\"message\":\"refused fixture-secret\"}}")))))
      (assert (eq (gemini-code-assist-setup-error-stage failure) :operation))
      (assert (= (provider-error-status failure) 7))
      (assert (search "refused" (provider-api-error-message failure)))
      (assert (not (search "fixture-secret" (provider-api-error-message failure))))))
  (let ((attempts 0)
        (cancel   (make-condition 'gemini-code-assist-test-cancelled)))
    (assert
     (eq cancel
         (handler-case
             (gemini-code-assist-run-setup
              (make-instance 'gemini-code-assist-test-provider)
              (lambda (method target payload)
                (declare (ignore method target payload))
                (incf attempts)
                (error cancel))
              :sleep-function (lambda (seconds) (error "Unexpected sleep: ~A" seconds)))
           (gemini-code-assist-test-cancelled (condition) condition))))
    (assert (= attempts 1))))

(defun run-gemini-code-assist-setup-tests ()
  "Run offline Code Assist setup fixtures."
  (gemini-code-assist-test--load)
  (gemini-code-assist-test--onboard)
  (gemini-code-assist-test--setup-errors)
  t)
