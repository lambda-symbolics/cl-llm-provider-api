(in-package #:cl-llm-provider-api/tests)

;;;; -- Provider Registry Tests --

(defun registry-tests--factory (&rest arguments)
  "Stand in for a provider factory."
  (declare (ignore arguments))
  :provider)

(defun registry-tests--registry (&rest arguments)
  "Return a registry with small defaults, adjusted by ARGUMENTS."
  (apply #'provider-registry-create
         (append arguments (list :default-context-window 1000
                                 :default-reasoning-efforts '("low" "high")))))

(defun registry-tests--names (registrations)
  "Return the names of REGISTRATIONS."
  (mapcar #'provider-registration-name registrations))

(defun registry-tests--signals-p (thunk)
  "Return true when THUNK signals PROVIDER-REGISTRY-ERROR."
  (handler-case (progn (funcall thunk) nil)
    (provider-registry-error ()
      t)))

(define-condition registry-tests-error (provider-registry-error)
  ()
  (:documentation "A host subclass of the registry condition."))

(defun test-registry-layers ()
  "Exercise source precedence, shadowing, removal and change notification."
  (let* ((changes 0)
         (registry (registry-tests--registry :change-function (lambda (registry)
                                                                 (declare (ignore registry))
                                                                 (incf changes)))))
    (provider-registry-register registry "Alpha" :models '("a-1") :factory #'registry-tests--factory
                                                 :source :builtin)
    (provider-registry-register registry "beta" :models '("b-1") :factory #'registry-tests--factory
                                                :source :builtin)
    (provider-registry-register registry "alpha" :models '("a-2") :factory #'registry-tests--factory
                                                 :source :user :description "User alpha")
    (check (equal (registry-tests--names (provider-registry-registrations registry))
                  '("alpha" "beta"))
           "a higher source shadows a name in first-registration order")
    (check (string= (provider-registration-description (provider-registry-find registry "ALPHA"))
                    "User alpha")
           "lookups ignore case and find the effective layer")
    (check (eq (provider-family-for-registration (provider-registry-find registry "beta")) :beta)
           "families derive from names and answer the library hook")
    (check (null (provider-registry-for-model registry "a-1"))
           "a shadowed layer's models are not served")
    (check (equal (provider-registry-model-identifiers registry) '("a-2" "b-1"))
           "model identifiers come from effective providers")
    (check (provider-registry-unregister registry "alpha" :source :user)
           "unregistering reports a removed layer")
    (check (string= (provider-registration-name (provider-registry-for-model registry "a-1")) "Alpha")
           "removing a layer uncovers the lower one")
    (check (not (provider-registry-unregister registry "alpha" :source :user))
           "unregistering twice removes nothing")
    (provider-registry-remove-source registry :builtin)
    (check (null (provider-registry-registrations registry)) "removing a source drops its layers")
    (check (= changes 6) "every change notifies the change function"))
  (let ((registry (registry-tests--registry)))
    (provider-registry-register registry "one" :models '("shared") :factory #'registry-tests--factory
                                               :source :site)
    (provider-registry-register registry "two" :models '("shared") :factory #'registry-tests--factory
                                               :source :site)
    (check (string= (provider-registration-name (provider-registry-for-model registry "shared")) "two")
           "the newer registration wins a shared model within one source")
    (provider-registry-register registry "one" :models '("shared") :factory #'registry-tests--factory
                                               :source :user)
    (check (string= (provider-registration-name (provider-registry-for-model registry "shared")) "one")
           "a higher source wins a shared model")
    (check (equal (provider-registry-model-identifiers registry) '("shared"))
           "a shared model is listed once")))

(defun test-registry-validation ()
  "Exercise model normalization, defaults and refused registrations."
  (let ((registry (registry-tests--registry)))
    (let ((plain (provider-model-create registry "plain"))
          (declared (provider-model-create registry '(:name "big" :context-window 9000
                                                      :reasoning-efforts ("max")))))
      (check (and (= (provider-model-context-window plain) 1000)
                  (not (provider-model-context-window-specified-p plain))
                  (equal (provider-model-reasoning-efforts plain) '("low" "high")))
             "an undeclared model takes the registry defaults")
      (check (and (= (provider-model-context-window declared) 9000)
                  (provider-model-context-window-specified-p declared)
                  (equal (provider-model-reasoning-efforts declared) '("max")))
             "declared metadata is kept"))
    (dolist (thunk (list (lambda () (provider-model-create registry ""))
                         (lambda () (provider-model-create registry '(:name "x" :context-window 0)))
                         (lambda () (provider-model-create registry '(:name "x" :reasoning-efforts ())))
                         (lambda () (provider-registry-register registry "" :models '("m")
                                                                           :factory #'identity))
                         (lambda () (provider-registry-register registry "p" :models '("m")))
                         (lambda () (provider-registry-register registry "p" :factory #'identity))
                         (lambda () (provider-registry-register registry "p" :models '("m" "m")
                                                                            :factory #'identity))
                         (lambda () (provider-registry-register registry "p" :models '("m")
                                                                            :factory #'identity
                                                                            :source :nowhere))
                         (lambda () (provider-registry-create :sources '(:a :a)))
                         (lambda () (provider-registry-refresh-models registry nil
                                                                      :provider-name "missing"))))
      (check (registry-tests--signals-p thunk) "invalid input is refused"))
    (let ((*provider-registry-error-class* 'registry-tests-error))
      (check (handler-case (progn (provider-model-create registry "") nil)
               (registry-tests-error () t))
             "hosts substitute the registry condition class"))))

(defun test-registry-discovery-and-cache ()
  "Exercise merged discovery, failures, retention, the cache and snapshots."
  (let* ((store nil)
         (discovered '((:name "declared" :context-window 7000) "found"))
         (failing-p nil)
         (registry (registry-tests--registry
                    :cache-read-function (lambda (context)
                                           (getf store context))
                    :cache-write-function (lambda (context form)
                                            (setf (getf store context) form))))
         (discovery (lambda (context)
                      (declare (ignore context))
                      (when failing-p
                        (error "discovery is down"))
                      discovered)))
    (provider-registry-register registry "dynamic" :models '("declared")
                                                   :factory #'registry-tests--factory
                                                   :model-discovery discovery
                                                   :model-discovery-endpoint "https://example.test/models")
    (check (null (provider-registry-refresh-models registry :home)) "a successful refresh has no failures")
    (let ((models (provider-registration-models (provider-registry-find registry "dynamic"))))
      (check (equal (mapcar #'provider-model-name models) '("declared" "found"))
             "declared models come first, discovered ones follow")
      (check (= (provider-model-context-window (first models)) 7000)
             "a declared model gains a discovered context window"))
    (check (eq (first (getf store :home)) :provider-model-cache)
           "a refresh writes the cache form for its context")
    (setf failing-p t)
    (let ((failures (provider-registry-refresh-models registry :home)))
      (check (and (= (length failures) 1)
                  (typep (first failures) 'provider-model-discovery-error)
                  (string= (provider-model-discovery-error-provider-name (first failures)) "dynamic"))
             "a failed refresh is returned, not signaled"))
    (check (provider-registry-model registry "found") "a failed refresh keeps the last models")
    (provider-registry-register registry "dynamic" :models '("declared")
                                                   :factory #'registry-tests--factory
                                                   :model-discovery discovery
                                                   :model-discovery-endpoint "https://example.test/models")
    (check (provider-registry-model registry "found")
           "re-registering an unchanged discovering provider keeps its discovered models")
    (let ((fresh (registry-tests--registry :cache-read-function (lambda (context)
                                                                  (getf store context)))))
      (provider-registry-register fresh "dynamic" :models '("declared")
                                                  :factory #'registry-tests--factory
                                                  :model-discovery discovery
                                                  :model-discovery-endpoint "https://example.test/models")
      (provider-registry-register fresh "moved" :models '("declared-elsewhere")
                                                :factory #'registry-tests--factory
                                                :model-discovery discovery
                                                :model-discovery-endpoint "https://other.test/models")
      (provider-registry-load-model-cache fresh :home)
      (check (provider-registry-model fresh "found") "a new registry adopts cached models")
      (check (= (length (provider-registration-models (provider-registry-find fresh "moved"))) 1)
             "a cache entry for another endpoint is ignored")
      (setf (getf store :old) (list* :provider-model-cache :version 1
                                     (rest (rest (rest (getf store :home))))))
      (let ((stale (registry-tests--registry :cache-read-function (lambda (context)
                                                                    (getf store context)))))
        (provider-registry-register stale "dynamic" :models '("declared")
                                                    :factory #'registry-tests--factory
                                                    :model-discovery discovery
                                                    :model-discovery-endpoint
                                                    "https://example.test/models")
        (provider-registry-load-model-cache stale :old)
        (check (null (provider-registry-model stale "found"))
               "a cache form of another version is ignored")))
    (let ((snapshot (provider-registry-snapshot registry)))
      (provider-registry-remove-source registry :runtime)
      (check (null (provider-registry-registrations registry)) "the registry was emptied")
      (provider-registry-restore registry snapshot)
      (check (provider-registry-model registry "found") "a snapshot restores layers and models"))))

(defun test-model-lists ()
  "Exercise model list decoding, context windows and model specification helpers."
  (let ((models (model-list-decode
                 "{\"data\": [{\"id\": \"a\", \"context_length\": 4096},
                              {\"id\": \"b\", \"top_provider\": {\"context_length\": \"8192\"}},
                              {\"id\": \"c\", \"n_ctx\": 2048.0},
                              {\"id\": \"d\", \"context_window\": -1},
                              {\"id\": \"skip\"}]}"
                 :entry-predicate (lambda (entry) (string/= (cl-llm-provider-api::json-get entry "id") "skip")))))
    (check (equal models '((:name "a" :context-window 4096) (:name "b" :context-window 8192)
                           (:name "c" :context-window 2048) (:name "d")))
           "context windows come from every known field and the predicate filters"))
  (dolist (body '("not json" "{\"data\": 1}" "{\"data\": [{\"name\": \"x\"}]}"))
    (check (handler-case (progn (model-list-decode body) nil)
             (provider-model-list-error () t))
           "an invalid model list ~S is refused" body))
  (check (and (string= (model-spec-name '(:name "x" :context-window 1)) "x")
              (equal (model-spec-rename '(:name "x" :context-window 1) "y")
                     '(:name "y" :context-window 1))
              (string= (model-spec-rename "x" "y") "y"))
         "model specifications are named and renamed"))

(defun run-registry-tests ()
  "Run the registry and model list tests."
  (test-registry-layers)
  (test-registry-validation)
  (test-registry-discovery-and-cache)
  (test-model-lists))
