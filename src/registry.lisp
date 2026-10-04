(in-package #:cl-llm-provider-api)

;;;; -- Provider Registry --

;;; A registry holds provider registrations in ordered source layers, such as
;;; builtin, site, user and runtime. Each name resolves to its highest-ranked
;;; layer, and later registrations win within one layer. A registration may
;;; declare models, discover them, or both: discovered models merge behind the
;;; declared ones, run under a per-registration lock, and persist through cache
;;; functions the host supplies, so the registry never chooses a file format
;;; or location.

(defparameter *provider-model-cache-version* 3
  "The portable version of the provider model cache form.")

(defparameter *provider-registry-error-class* 'provider-registry-error
  "The condition class signaled for invalid registrations and lookups.
Hosts may name a subclass carrying their own condition protocol.")


;;;; -- Classes --

(defclass provider-model ()
  ((name
    :initarg :name
    :reader provider-model-name
    :type string
    :documentation "The model identifier accepted by a provider.")
   (description
    :initarg :description
    :initform ""
    :reader provider-model-description
    :type string
    :documentation "The optional user-visible model description.")
   (context-window
    :initarg :context-window
    :reader provider-model-context-window
    :type (integer 1)
    :documentation "The model context window in tokens.")
   (context-window-specified-p
    :initarg :context-window-specified-p
    :initform nil
    :reader provider-model-context-window-specified-p
    :type boolean
    :documentation
    "True when CONTEXT-WINDOW was declared or discovered, not filled from a default.")
   (reasoning-efforts
    :initarg :reasoning-efforts
    :reader provider-model-reasoning-efforts
    :type list
    :documentation "The reasoning effort names offered for this model."))
  (:documentation "Metadata describing one model exposed by a registered provider."))

(defmethod initialize-instance :after
    ((model provider-model)
     &key (context-window nil context-window-p)
          (context-window-specified-p nil specified-p))
  "Treat an explicit context window as specified unless told otherwise."
  (declare (ignore context-window context-window-specified-p))
  (unless specified-p
    (setf (slot-value model 'context-window-specified-p) context-window-p)))

(defclass provider-registration ()
  ((name
    :initarg :name
    :reader provider-registration-name
    :type string
    :documentation "The stable user-visible provider name.")
   (description
    :initarg :description
    :reader provider-registration-description
    :type string
    :documentation "The user-visible provider description.")
   (family
    :initarg :family
    :reader provider-registration-family
    :type keyword
    :documentation "The conversation family keyword used for private item filtering.")
   (models
    :initarg :models
    :reader provider-registration-models
    :type list
    :documentation "The ordered effective model metadata exposed by this provider.")
   (declared-models
    :initarg :declared-models
    :reader provider-registration-declared-models
    :type list
    :documentation "The static model metadata declared by this provider.")
   (discovered-models
    :initarg :discovered-models
    :initform nil
    :reader provider-registration-discovered-models
    :type list
    :documentation "The last successful dynamic model metadata for this provider.")
   (model-discovery
    :initarg :model-discovery
    :initform nil
    :reader provider-registration-model-discovery
    :type (or null function)
    :documentation "The function of one context returning current model specifications.")
   (model-discovery-endpoint
    :initarg :model-discovery-endpoint
    :initform nil
    :reader provider-registration-model-discovery-endpoint
    :type (or null string)
    :documentation "The endpoint identifying discovered models in the cache, when declared.")
   (model-discovery-endpoint-resolver
    :initarg :model-discovery-endpoint-resolver
    :initform nil
    :reader provider-registration-model-discovery-endpoint-resolver
    :type (or null function)
    :documentation "The optional zero-argument function returning the current discovery endpoint.")
   (model-discovery-lock
    :initform (bordeaux-threads:make-recursive-lock "Provider model discovery")
    :reader provider-registration-model-discovery-lock
    :documentation "The lock serializing discovery requests for this provider.")
   (factory
    :initarg :factory
    :reader provider-registration-factory
    :type function
    :documentation "The host function creating a provider.")
   (authenticator
    :initarg :authenticator
    :initform nil
    :reader provider-registration-authenticator
    :type (or null function)
    :documentation "The optional host function implementing provider authentication.")
   (protocol
    :initarg :protocol
    :initform :custom
    :reader provider-registration-protocol
    :type keyword
    :documentation "The wire protocol label shown in provider diagnostics.")
   (endpoint
    :initarg :endpoint
    :initform nil
    :reader provider-registration-endpoint
    :type (or null string)
    :documentation "The provider endpoint, when one is declared by metadata.")
   (source
    :initarg :source
    :reader provider-registration-source
    :type keyword
    :documentation "The source layer that supplied this registration.")
   (sequence
    :initarg :sequence
    :reader provider-registration-sequence
    :type (integer 0)
    :documentation "The monotonic registration order within its registry."))
  (:documentation "One provider registration layer and its model metadata."))

(defclass provider-registry ()
  ((sources
    :initarg :sources
    :reader provider-registry-sources
    :type list
    :documentation "The registration source keywords from lowest to highest precedence.")
   (default-context-window
    :initarg :default-context-window
    :reader provider-registry-default-context-window
    :type (integer 1)
    :documentation "The context window of models that declare none.")
   (default-reasoning-efforts
    :initarg :default-reasoning-efforts
    :reader provider-registry-default-reasoning-efforts
    :type list
    :documentation "The reasoning efforts of models that declare none.")
   (cache-read-function
    :initarg :cache-read-function
    :reader provider-registry-cache-read-function
    :type (or null function)
    :documentation "NIL, or a function of one context returning the cache form or NIL.")
   (cache-write-function
    :initarg :cache-write-function
    :reader provider-registry-cache-write-function
    :type (or null function)
    :documentation "NIL, or a function of a context and a cache form that stores the form.")
   (change-function
    :initarg :change-function
    :reader provider-registry-change-function
    :type (or null function)
    :documentation "NIL, or a function of the registry called after effective models change.")
   (registrations
    :initform nil
    :accessor provider-registry--registrations
    :documentation "Every registration layer, shadowed ones included.")
   (sequence
    :initform 0
    :accessor provider-registry--sequence
    :documentation "The last registration sequence number issued.")
   (lock
    :initform (bordeaux-threads:make-recursive-lock "Provider registry")
    :reader provider-registry--lock
    :documentation "Guards the registration layers and their published models.")
   (cache-lock
    :initform (bordeaux-threads:make-lock "Provider model cache")
    :reader provider-registry--cache-lock
    :documentation "Serializes cache read-modify-write cycles."))
  (:documentation "Layered provider registrations with merged and cached model metadata."))


;;;; -- Conditions --

(define-condition provider-registry-error (provider-api-error)
  ()
  (:documentation "A provider registration or registry lookup was invalid."))

(define-condition provider-model-discovery-error (provider-api-error)
  ((provider-name
    :initarg :provider-name
    :reader provider-model-discovery-error-provider-name
    :type string
    :documentation "The provider whose model list could not be discovered.")
   (cause
    :initarg :cause
    :reader provider-model-discovery-error-cause
    :documentation "The underlying request or response failure."))
  (:documentation "A provider's dynamic model list could not be refreshed."))

(defun registry--fail (control &rest arguments)
  "Signal *PROVIDER-REGISTRY-ERROR-CLASS* with the message CONTROL formats."
  (error *provider-registry-error-class* :message (apply #'format nil control arguments)))


;;;; -- Family Hook --

(defmethod provider-family-for-registration ((registration provider-registration))
  "Return the family REGISTRATION declares."
  (provider-registration-family registration))


;;;; -- Registry Construction --

(defun provider-name-family (name)
  "Return the family keyword a registration named NAME takes when it declares none.

Every character of NAME that is not alphanumeric becomes a hyphen."
  (intern (string-upcase (substitute-if #\- (lambda (character) (not (alphanumericp character)))
                                        name))
          '#:keyword))

(defun provider-registry-create (&key (sources '(:builtin :site :user :runtime))
                                      (default-context-window 128000)
                                      (default-reasoning-efforts '("low" "medium" "high"))
                                      cache-read-function cache-write-function
                                      change-function)
  "Return an empty registry layering SOURCES from lowest to highest precedence.

Models that declare no context window or reasoning efforts take
DEFAULT-CONTEXT-WINDOW and DEFAULT-REASONING-EFFORTS. CACHE-READ-FUNCTION and
CACHE-WRITE-FUNCTION, when given, load and store the portable model cache form
for a host context, such as a configuration naming a file. CHANGE-FUNCTION is
called with the registry whenever the effective models may have changed."
  (unless (and sources (every #'keywordp sources)
               (= (length sources) (length (remove-duplicates sources))))
    (registry--fail "Registry sources must be distinct keywords."))
  (unless (typep default-context-window '(integer 1))
    (registry--fail "The default context window must be a positive integer."))
  (unless (registry--reasoning-efforts-p default-reasoning-efforts)
    (registry--fail "The default reasoning efforts must be nonempty strings."))
  (dolist (function (list cache-read-function cache-write-function change-function))
    (unless (or (null function) (functionp function))
      (registry--fail "Registry hooks must be functions.")))
  (make-instance 'provider-registry
                 :sources (copy-list sources)
                 :default-context-window default-context-window
                 :default-reasoning-efforts (copy-list default-reasoning-efforts)
                 :cache-read-function cache-read-function
                 :cache-write-function cache-write-function
                 :change-function change-function))

(defun provider-model-create (registry spec)
  "Normalize model SPEC into metadata using REGISTRY's defaults.

SPEC is a model name, an existing PROVIDER-MODEL, or a property list with
:NAME, :DESCRIPTION, :CONTEXT-WINDOW and :REASONING-EFFORTS. An omitted
:CONTEXT-WINDOW takes the default and does not count as specified."
  (etypecase spec
    (provider-model
     spec)
    (string
     (unless (plusp (length spec))
       (registry--fail "Provider model names must not be empty."))
     (make-instance 'provider-model
                    :name spec
                    :context-window (provider-registry-default-context-window registry)
                    :context-window-specified-p nil
                    :reasoning-efforts
                    (copy-list (provider-registry-default-reasoning-efforts registry))))
    (cons
     (let ((name (getf spec :name)))
       (unless (and (stringp name) (plusp (length name)))
         (registry--fail "Provider model metadata needs a nonempty :name: ~S." spec))
       (multiple-value-bind (context-window specified-p) (registry--property spec :context-window)
         (unless (or (not specified-p) (typep context-window '(integer 1)))
           (registry--fail "Provider model ~A needs a positive :context-window." name))
         (let ((reasoning-efforts
                 (getf spec :reasoning-efforts
                       (provider-registry-default-reasoning-efforts registry))))
           (unless (registry--reasoning-efforts-p reasoning-efforts)
             (registry--fail "Provider model ~A has invalid :reasoning-efforts." name))
           (make-instance 'provider-model
                          :name name
                          :description (or (getf spec :description) "")
                          :context-window (if specified-p
                                              context-window
                                              (provider-registry-default-context-window registry))
                          :context-window-specified-p specified-p
                          :reasoning-efforts (copy-list reasoning-efforts))))))))


;;;; -- Registration --

(defun provider-registry-register
    (registry name &key description family models factory authenticator (protocol :custom)
                     endpoint model-discovery model-discovery-endpoint
                     model-discovery-endpoint-resolver
                     (source (first (last (provider-registry-sources registry)))))
  "Register provider NAME in REGISTRY's SOURCE layer and return NAME.

FACTORY and AUTHENTICATOR are host functions the registry only stores.
MODEL-DISCOVERY, when given, receives the context passed to
PROVIDER-REGISTRY-REFRESH-MODELS and returns model specifications.
MODEL-DISCOVERY-ENDPOINT, or the zero-argument
MODEL-DISCOVERY-ENDPOINT-RESOLVER, identifies discovered models in the cache.
A registration replaces only the same name in the same source and shadows
lower sources. Replacing an unchanged discovering registration keeps the models
it already discovered."
  (unless (and (stringp name) (plusp (length name)))
    (registry--fail "Provider names must not be empty."))
  (unless (functionp factory)
    (registry--fail "Provider ~A needs a callable :factory." name))
  (unless (or (null authenticator) (functionp authenticator))
    (registry--fail "Provider ~A has a non-callable :authenticator." name))
  (unless (or (null model-discovery) (functionp model-discovery))
    (registry--fail "Provider ~A has a non-callable :model-discovery." name))
  (unless (or (null model-discovery-endpoint-resolver)
              (functionp model-discovery-endpoint-resolver))
    (registry--fail "Provider ~A has a non-callable model discovery endpoint resolver." name))
  (unless (or model-discovery (and (listp models) models))
    (registry--fail "Provider ~A needs :models or a callable :model-discovery." name))
  (registry--source-rank registry source)
  (unless (or (null endpoint) (and (stringp endpoint) (plusp (length endpoint))))
    (registry--fail "Provider ~A has an invalid endpoint." name))
  (unless (or (null model-discovery-endpoint)
              (and (stringp model-discovery-endpoint) (plusp (length model-discovery-endpoint))))
    (registry--fail "Provider ~A has an invalid model discovery endpoint." name))
  (let* ((declared-models (registry--normalize-models registry models
                                                      :allow-empty-p (functionp model-discovery)))
         (previous (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
                     (registry--layer registry name source)))
         (retained-models
           (if (and model-discovery
                    previous
                    (null model-discovery-endpoint-resolver)
                    (null (provider-registration-model-discovery-endpoint-resolver previous))
                    (equal endpoint (provider-registration-endpoint previous))
                    (equal model-discovery-endpoint
                           (provider-registration-model-discovery-endpoint previous)))
               (copy-list (provider-registration-discovered-models previous))
               nil))
         (registration
           (make-instance 'provider-registration
                          :name name
                          :description (or description name)
                          :family (or family (provider-name-family name))
                          :models (registry--merge-models registry declared-models retained-models)
                          :declared-models declared-models
                          :discovered-models retained-models
                          :model-discovery model-discovery
                          :model-discovery-endpoint model-discovery-endpoint
                          :model-discovery-endpoint-resolver model-discovery-endpoint-resolver
                          :factory factory
                          :authenticator authenticator
                          :protocol protocol
                          :endpoint endpoint
                          :source source
                          :sequence 0)))
    (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
      (setf (slot-value registration 'sequence) (incf (provider-registry--sequence registry))
            (provider-registry--registrations registry)
            (append (remove-if (lambda (candidate)
                                 (registry--same-layer-p candidate name source))
                               (provider-registry--registrations registry))
                    (list registration)))
      (registry--changed registry))
    name))

(defun provider-registry-unregister (registry name &key (source (first (last (provider-registry-sources registry)))))
  "Remove NAME from REGISTRY's SOURCE layer and return whether it was present."
  (registry--source-rank registry source)
  (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
    (let ((before (length (provider-registry--registrations registry))))
      (setf (provider-registry--registrations registry)
            (remove-if (lambda (candidate) (registry--same-layer-p candidate name source))
                       (provider-registry--registrations registry)))
      (registry--changed registry)
      (< (length (provider-registry--registrations registry)) before))))

(defun provider-registry-remove-source (registry source)
  "Remove every registration REGISTRY holds in SOURCE."
  (registry--source-rank registry source)
  (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
    (setf (provider-registry--registrations registry)
          (remove source (provider-registry--registrations registry)
                  :key #'provider-registration-source))
    (registry--changed registry))
  nil)


;;;; -- Effective Views --

(defun provider-registry-registrations (registry)
  "Return REGISTRY's effective registrations in first-registration order."
  (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
    (let ((order nil)
          (winners (make-hash-table :test #'equal)))
      (dolist (registration (sort (copy-list (provider-registry--registrations registry))
                                  #'< :key #'provider-registration-sequence))
        (let* ((key (registry--key (provider-registration-name registration)))
               (winner (gethash key winners)))
          (unless winner
            (push key order))
          (when (or (null winner)
                    (>= (registry--source-rank registry (provider-registration-source registration))
                        (registry--source-rank registry (provider-registration-source winner))))
            (setf (gethash key winners) registration))))
      (mapcar (lambda (key) (gethash key winners)) (nreverse order)))))

(defun provider-registry-find (registry name)
  "Return REGISTRY's effective registration named NAME, ignoring case, or NIL."
  (when (and (stringp name) (plusp (length name)))
    (find (registry--key name) (provider-registry-registrations registry)
          :key (lambda (registration) (registry--key (provider-registration-name registration)))
          :test #'string=)))

(defun provider-registry-for-model (registry model)
  "Return the effective registration serving MODEL, or NIL.

When several providers offer MODEL, the higher source wins, then the newer
registration."
  (when (and (stringp model) (plusp (length model)))
    (find-if (lambda (registration)
               (find model (provider-registration-models registration)
                     :key #'provider-model-name :test #'string=))
             (sort (provider-registry-registrations registry)
                   (lambda (left right) (registry--precedes-p registry left right))))))

(defun provider-registry-model (registry model)
  "Return the effective metadata for MODEL, or NIL."
  (let ((registration (provider-registry-for-model registry model)))
    (and registration
         (find model (provider-registration-models registration)
               :key #'provider-model-name :test #'string=))))

(defun provider-registry-model-identifiers (registry)
  "Return each model identifier REGISTRY's effective providers serve, once."
  (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
    (let ((seen (make-hash-table :test #'equal))
          (models nil))
      (dolist (registration (provider-registry-registrations registry))
        (dolist (model (provider-registration-models registration))
          (let ((name (provider-model-name model)))
            (when (and (not (gethash name seen))
                       (eq registration (provider-registry-for-model registry name)))
              (setf (gethash name seen) t)
              (push name models)))))
      (nreverse models))))


;;;; -- Discovery and Cache --

(defun provider-registry-load-model-cache (registry context)
  "Adopt the cached discovered models REGISTRY's cache read function returns for CONTEXT."
  (let ((entries (bordeaux-threads:with-lock-held ((provider-registry--cache-lock registry))
                   (registry--read-cache registry context))))
    (when entries
      (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
        (dolist (registration (provider-registry--registrations registry))
          (when (provider-registration-model-discovery registration)
            (let ((entry (registry--cache-entry-for registration entries)))
              (when entry
                (registry--publish-discovered registry registration
                                              (copy-list (getf entry :models)))))))
        (registry--changed registry))))
  nil)

(defun provider-registry-refresh-models (registry context &key provider-name)
  "Rediscover models for CONTEXT and return the failures as conditions.

Every effective discovering registration is refreshed, or only PROVIDER-NAME's.
A failure keeps the last successful models and becomes a
PROVIDER-MODEL-DISCOVERY-ERROR in the returned list; an unknown PROVIDER-NAME
signals."
  (let ((registrations (if provider-name
                           (list (or (provider-registry-find registry provider-name)
                                     (registry--fail "Unknown provider ~A." provider-name)))
                           (provider-registry-registrations registry)))
        (failures nil))
    (dolist (registration registrations (nreverse failures))
      (when (provider-registration-model-discovery registration)
        (handler-case (registry--refresh-registration registry registration context)
          (error (cause)
            (push (make-condition 'provider-model-discovery-error
                                  :message (format nil "Could not discover models for provider ~A."
                                                   (provider-registration-name registration))
                                  :provider-name (provider-registration-name registration)
                                  :cause cause)
                  failures)))))))


;;;; -- Snapshots --

(defun provider-registry-snapshot (registry)
  "Return an exact snapshot of REGISTRY's layers and their published models."
  (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
    (list :registrations (copy-list (provider-registry--registrations registry))
          :models (loop for registration in (provider-registry--registrations registry)
                        collect (list registration
                                      (copy-list (provider-registration-models registration))
                                      (copy-list (provider-registration-discovered-models
                                                  registration))))
          :sequence (provider-registry--sequence registry))))

(defun provider-registry-restore (registry snapshot)
  "Restore REGISTRY's layers and published models from SNAPSHOT."
  (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
    (loop for (registration models discovered) in (getf snapshot :models)
          do (setf (slot-value registration 'models) (copy-list models)
                   (slot-value registration 'discovered-models) (copy-list discovered)))
    (setf (provider-registry--registrations registry) (copy-list (getf snapshot :registrations))
          (provider-registry--sequence registry) (getf snapshot :sequence))
    (registry--changed registry))
  nil)


;;;; -- Private Helpers --

(defun registry--key (name)
  "Return the case-insensitive key of provider NAME."
  (string-downcase name))

(defun registry--property (plist indicator)
  "Return INDICATOR's value in PLIST and whether it is present."
  (loop for (key value) on plist by #'cddr
        when (eq key indicator)
          return (values value t)
        finally (return (values nil nil))))

(defun registry--reasoning-efforts-p (value)
  "Return true when VALUE is a nonempty list of nonempty strings."
  (and (listp value)
       value
       (every (lambda (effort) (and (stringp effort) (plusp (length effort)))) value)
       t))

(defun registry--source-rank (registry source)
  "Return SOURCE's precedence rank in REGISTRY, signaling for an unknown source."
  (or (position source (provider-registry-sources registry))
      (registry--fail "Unknown provider registration source ~S." source)))

(defun registry--precedes-p (registry left right)
  "Return true when registration LEFT takes precedence over RIGHT."
  (let ((left-rank (registry--source-rank registry (provider-registration-source left)))
        (right-rank (registry--source-rank registry (provider-registration-source right))))
    (or (> left-rank right-rank)
        (and (= left-rank right-rank)
             (> (provider-registration-sequence left) (provider-registration-sequence right))))))

(defun registry--same-layer-p (registration name source)
  "Return true when REGISTRATION is NAME's layer in SOURCE."
  (and (eq (provider-registration-source registration) source)
       (string= (registry--key (provider-registration-name registration)) (registry--key name))))

(defun registry--layer (registry name source)
  "Return NAME's registration in REGISTRY's SOURCE layer, or NIL."
  (find-if (lambda (candidate) (registry--same-layer-p candidate name source))
           (provider-registry--registrations registry)))

(defun registry--normalize-models (registry models &key allow-empty-p)
  "Normalize and validate the ordered model specifications MODELS."
  (unless (and (listp models) (or models allow-empty-p))
    (registry--fail "A provider registration needs at least one model."))
  (let ((seen (make-hash-table :test #'equal))
        (normalized nil))
    (dolist (spec models (nreverse normalized))
      (let* ((model (provider-model-create registry spec))
             (name (provider-model-name model)))
        (when (gethash name seen)
          (registry--fail "Provider registration repeats model ~A." name))
        (setf (gethash name seen) t)
        (push model normalized)))))

(defun registry--merge-models (registry declared-models discovered-models)
  "Return DECLARED-MODELS followed by the other DISCOVERED-MODELS.

A declared model keeps its description and reasoning efforts and gains the
discovered context window only when it declares none."
  (let* ((discovered (registry--normalize-models registry discovered-models :allow-empty-p t))
         (by-name (make-hash-table :test #'equal))
         (seen (make-hash-table :test #'equal))
         (merged nil))
    (dolist (model discovered)
      (setf (gethash (provider-model-name model) by-name) model))
    (dolist (declared declared-models)
      (let ((match (gethash (provider-model-name declared) by-name)))
        (setf (gethash (provider-model-name declared) seen) t)
        (push (if (and match
                       (not (provider-model-context-window-specified-p declared))
                       (provider-model-context-window-specified-p match))
                  (make-instance 'provider-model
                                 :name (provider-model-name declared)
                                 :description (provider-model-description declared)
                                 :context-window (provider-model-context-window match)
                                 :context-window-specified-p t
                                 :reasoning-efforts
                                 (copy-list (provider-model-reasoning-efforts declared)))
                  declared)
              merged)))
    (dolist (model discovered)
      (unless (gethash (provider-model-name model) seen)
        (setf (gethash (provider-model-name model) seen) t)
        (push model merged)))
    (nreverse merged)))

(defun registry--publish-discovered (registry registration discovered)
  "Install DISCOVERED and the merged effective models on REGISTRATION."
  (setf (slot-value registration 'discovered-models) discovered
        (slot-value registration 'models)
        (registry--merge-models registry (provider-registration-declared-models registration)
                                discovered)))

(defun registry--refresh-registration (registry registration context)
  "Discover REGISTRATION's models for CONTEXT, publish them while it is registered, and cache them."
  (bordeaux-threads:with-recursive-lock-held ((provider-registration-model-discovery-lock
                                               registration))
    (let ((discovered (registry--normalize-models
                       registry
                       (funcall (provider-registration-model-discovery registration) context)
                       :allow-empty-p t))
          (published-p nil))
      (bordeaux-threads:with-recursive-lock-held ((provider-registry--lock registry))
        (when (member registration (provider-registry--registrations registry) :test #'eq)
          (registry--publish-discovered registry registration discovered)
          (setf published-p t)
          (registry--changed registry)))
      (when published-p
        (registry--write-cache registry registration context discovered))
      (provider-registration-models registration))))

(defun registry--discovery-endpoint (registration)
  "Return REGISTRATION's current discovery cache identity."
  (let ((resolver (provider-registration-model-discovery-endpoint-resolver registration)))
    (if resolver
        (let ((endpoint (funcall resolver)))
          (unless (and (stringp endpoint) (plusp (length endpoint)))
            (registry--fail "Provider ~A resolved an invalid model discovery endpoint."
                            (provider-registration-name registration)))
          endpoint)
        (provider-registration-model-discovery-endpoint registration))))

(defun registry--cache-entry-for (registration entries)
  "Return the cache entry in ENTRIES matching REGISTRATION's name and discovery endpoint."
  (let ((endpoint (registry--discovery-endpoint registration))
        (key (registry--key (provider-registration-name registration))))
    (find-if (lambda (entry)
               (and (string= key (registry--key (getf entry :provider-name)))
                    (equal endpoint (getf entry :discovery-endpoint))))
             entries)))

(defun registry--read-cache (registry context)
  "Return the valid normalized cache entries for CONTEXT, or NIL."
  (let ((function (provider-registry-cache-read-function registry)))
    (when function
      (handler-case
          (let ((form (funcall function context)))
            (when (and (consp form)
                       (eq (first form) :provider-model-cache)
                       (eql (getf (rest form) :version) *provider-model-cache-version*)
                       (listp (getf (rest form) :providers)))
              (loop for entry in (getf (rest form) :providers)
                    for name = (and (listp entry) (getf entry :provider-name))
                    for endpoint = (and (listp entry) (getf entry :discovery-endpoint))
                    for models = (and (listp entry) (getf entry :models))
                    when (and (stringp name) (plusp (length name))
                              (or (null endpoint) (and (stringp endpoint) (plusp (length endpoint))))
                              (listp models))
                      collect (list :provider-name name
                                    :discovery-endpoint endpoint
                                    :models (registry--normalize-models registry models
                                                                        :allow-empty-p t)))))
        (error ()
          nil)))))

(defun registry--model-cache-form (model)
  "Return MODEL's portable cache form."
  (append (list :name (provider-model-name model)
                :description (provider-model-description model))
          (when (provider-model-context-window-specified-p model)
            (list :context-window (provider-model-context-window model)))
          (list :reasoning-efforts (copy-list (provider-model-reasoning-efforts model)))))

(defun registry--write-cache (registry registration context models)
  "Best effort, replace REGISTRATION's cache entry for CONTEXT with MODELS."
  (let ((function (provider-registry-cache-write-function registry)))
    (when function
      (handler-case
          (bordeaux-threads:with-lock-held ((provider-registry--cache-lock registry))
            (let* ((endpoint (registry--discovery-endpoint registration))
                   (key (registry--key (provider-registration-name registration)))
                   (entries (cons (list :provider-name (provider-registration-name registration)
                                        :discovery-endpoint endpoint
                                        :models models)
                                  (remove-if (lambda (entry)
                                               (and (string= key (registry--key
                                                                  (getf entry :provider-name)))
                                                    (equal endpoint
                                                           (getf entry :discovery-endpoint))))
                                             (registry--read-cache registry context)))))
              (funcall function context
                       (list :provider-model-cache
                             :version *provider-model-cache-version*
                             :providers
                             (loop for entry in entries
                                   collect (list :provider-name (getf entry :provider-name)
                                                 :discovery-endpoint (getf entry :discovery-endpoint)
                                                 :models (mapcar #'registry--model-cache-form
                                                                 (getf entry :models))))))))
        (error ()
          nil))))
  nil)

(defun registry--changed (registry)
  "Tell REGISTRY's change function that effective models may have changed."
  (let ((function (provider-registry-change-function registry)))
    (when function
      (funcall function registry)))
  nil)
