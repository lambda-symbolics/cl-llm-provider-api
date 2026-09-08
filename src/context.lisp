(in-package #:cl-llm-provider-api)

(define-condition context-contribution-error
    (provider-api-error)
    nil
  (:documentation "A request context contribution violates its value contract."))

(deftype context-contribution-lifetime ()
  "The request-local activation period of one contribution."
  '(member :while-relevant :next-request :turn :until-success))

(deftype context-contribution-class ()
  "Whether a contribution bypasses the advisory budget."
  '(member :mandatory :advice))

(defun context--non-empty-string-p (value)
  "Return true for a nonempty string."
  (and (stringp value) (plusp (length value))))

(defparameter *context-contribution-identifier-limit*
  128
  "The maximum characters in a context contribution identifier.")

(defparameter *context-contribution-instruction-limit*
  4000
  "The maximum characters in one advisory request-local instruction.")

(defparameter *context-mandatory-instruction-limit*
  (* 128 1024)
  "The maximum characters in one mandatory request-local instruction.")

(defparameter *context-contribution-evidence-limit*
  2000
  "The maximum characters in one untrusted evidence value.")

(defparameter *context-contribution-reference-limit*
  32
  "The maximum supersession references on one contribution.")

(defclass context-contribution nil
          ((identifier :initarg :identifier :reader context-contribution-identifier
            :type string :documentation
            "The stable identity used for inspection and supersession.")
           (instruction :initarg :instruction :reader context-contribution-instruction
            :type string :documentation
            "Trusted advice rendered only into the current request.")
           (evidence :initarg :evidence :initform nil :reader
            context-contribution-evidence :type (or null string) :documentation
            "Optional untrusted data supporting the instruction.")
           (priority :initarg :priority :initform 0 :reader
            context-contribution-priority :type integer :documentation
            "The budget and rendering priority; larger values matter more.")
           (lifetime :initarg :lifetime :initform ':while-relevant :reader
            context-contribution-lifetime :type context-contribution-lifetime
            :documentation
            "The declared period for which the advice remains relevant.")
           (class :initarg :class :initform ':advice :reader context-contribution-class
            :type context-contribution-class :documentation
            "Whether the contribution competes for the advice budget.")
           (deduplication-key :initarg :deduplication-key :initform nil :reader
            context-contribution-deduplication-key :type (or null string)
            :documentation
            "The optional semantic identity shared by equivalent advice.")
           (supersedes :initarg :supersedes :initform nil :reader
            context-contribution-supersedes :type list :documentation
            "Contribution identifiers or deduplication keys replaced by this one.")
           (conflict-group :initarg :conflict-group :initform nil :reader
            context-contribution-conflict-group :type (or null string) :documentation
            "The optional group in which only the strongest advice applies.")
           (contributor :initarg :contributor :initform "unknown" :reader
            context-contribution-contributor :type string :documentation
            "The registration that produced this contribution.")
           (source :initarg :source :initform ':runtime :reader
            context-contribution-source :type keyword :documentation
            "The built-in, user, or runtime origin of the contributor."))
          (:documentation
           "One structured instruction that never enters conversation history."))

(defun context--validate-identifier (value field)
  "Return non-empty string VALUE after validating bounded FIELD identity."
  (unless
      (and (context--non-empty-string-p value)
           (<= (length value) *context-contribution-identifier-limit*))
    (error 'context-contribution-error :message
           (format nil "~A must contain 1 to ~D characters." field
                   *context-contribution-identifier-limit*)))
  value)

(defun context--validate-references (references)
  "Return a copied, unique list of bounded contribution REFERENCES."
  (unless
      (handler-case
       (let ((length (list-length references)))
         (and (integerp length) (<= length *context-contribution-reference-limit*)
              (every
               (lambda (reference)
                 (and (context--non-empty-string-p reference)
                      (<= (length reference) *context-contribution-identifier-limit*)))
               references)))
       (type-error nil nil))
    (error 'context-contribution-error :message
           "Context supersession references must be a bounded list of strings."))
  (remove-duplicates (copy-list references) :test #'string= :from-end t))

(defun make-context-contribution
    (&key identifier instruction evidence (priority 0) (lifetime ':while-relevant)
       (class ':advice) deduplication-key supersedes conflict-group)
  "Return one validated request-local context contribution."
  (context--validate-identifier identifier "Context contribution identifier")
  (unless (integerp priority)
    (error 'context-contribution-error :message "Context priority must be an integer."))
  (unless (typep class 'context-contribution-class)
    (error 'context-contribution-error :message
           (format nil "Unsupported context class ~S." class)))
  (let ((instruction-limit
         (if (eq class ':mandatory)
             *context-mandatory-instruction-limit*
             *context-contribution-instruction-limit*)))
    (unless
        (and (context--non-empty-string-p instruction)
             (<= (length instruction) instruction-limit))
      (error 'context-contribution-error :message
             (format nil "Context instruction must contain 1 to ~D characters."
                     instruction-limit))))
  (unless
      (or (null evidence)
          (and (stringp evidence)
               (<= (length evidence) *context-contribution-evidence-limit*)))
    (error 'context-contribution-error :message
           (format nil "Context evidence must contain at most ~D characters."
                   *context-contribution-evidence-limit*)))
  (unless (typep lifetime 'context-contribution-lifetime)
    (error 'context-contribution-error :message
           (format nil "Unsupported context lifetime ~S." lifetime)))
  (when deduplication-key
    (context--validate-identifier deduplication-key "Context deduplication key"))
  (when conflict-group
    (context--validate-identifier conflict-group "Context conflict group"))
  (make-instance 'context-contribution :identifier identifier :instruction instruction
                 :evidence evidence :priority priority :lifetime lifetime :class class
                 :deduplication-key deduplication-key :supersedes
                 (context--validate-references supersedes) :conflict-group
                 conflict-group))

(defun context--contribution-key (contribution)
  "Return CONTRIBUTION's semantic deduplication identity."
  (or (context-contribution-deduplication-key contribution)
      (context-contribution-identifier contribution)))

(defun context--importance-greater-p (left right)
  "Return true when LEFT should survive a conflict before RIGHT."
  (or
   (and (eq (context-contribution-class left) ':mandatory)
        (not (eq (context-contribution-class right) ':mandatory)))
   (and (eq (context-contribution-class left) (context-contribution-class right))
        (> (context-contribution-priority left)
           (context-contribution-priority right)))))

(defun context--deduplicate (contributions)
  "Return CONTRIBUTIONS with the strongest value retained for each semantic key."
  (let ((selected nil))
    (dolist (contribution contributions)
      (let* ((key (context--contribution-key contribution))
             (existing
              (find key selected :test #'string= :key #'context--contribution-key)))
        (cond ((null existing) (setf selected (append selected (list contribution))))
              ((context--importance-greater-p contribution existing)
               (setf selected (substitute contribution existing selected))))))
    selected))

(defun context--apply-supersession (contributions)
  "Remove advisory contributions explicitly superseded by another contribution."
  (let ((superseded
         (remove-duplicates
          (mapcan
           (lambda (contribution)
             (copy-list (context-contribution-supersedes contribution)))
           contributions)
          :test #'string=)))
    (remove-if
     (lambda (contribution)
       (and (eq (context-contribution-class contribution) ':advice)
            (or
             (member (context-contribution-identifier contribution) superseded :test
                     #'string=)
             (member (context--contribution-key contribution) superseded :test
                     #'string=))))
     contributions)))

(defun context--resolve-conflicts (contributions)
  "Keep the strongest contribution only within explicit conflict groups."
  (let ((selected nil))
    (dolist (contribution contributions)
      (let* ((group (context-contribution-conflict-group contribution))
             (existing
              (and group
                   (find-if
                    (lambda (candidate)
                      (let ((candidate-group
                             (context-contribution-conflict-group candidate)))
                        (and candidate-group (string= group candidate-group))))
                    selected))))
        (cond ((null existing) (setf selected (append selected (list contribution))))
              ((context--importance-greater-p contribution existing)
               (setf selected (substitute contribution existing selected))))))
    selected))

(defun context-contribution-token-estimate (contribution)
  "Return a conservative character-based token estimate for CONTRIBUTION."
  (ceiling (+ 24 (length (context-contribution-instruction contribution))
              (length (or (context-contribution-evidence contribution) "")))
           4))


;;;; -- Explicit Resolution and Delivery State --

(defclass context-resolver ()
  ((lock :initform (bordeaux-threads:make-lock "Request context receipts")
         :reader context-resolver--lock
         :documentation "Serializes activation and receipt updates.")
   (activations :initform (make-hash-table :test #'equal)
                :reader context-resolver--activations
                :documentation "Session and semantic keys mapped to activation tokens.")
   (delivered :initform (make-hash-table :test #'equal)
              :reader context-resolver--delivered
              :documentation "Successfully delivered activation tokens by key."))
  (:documentation "Explicit session-isolated next-request receipt state."))

(defclass context-selection ()
  ((resolver :initarg :resolver :reader context-selection--resolver
             :documentation "The resolver that owns this selection's receipts.")
   (contributions :initarg :contributions :reader context-selection-contributions
                  :documentation "Selected mandatory and budgeted advisory values.")
   (omitted :initarg :omitted :reader context-selection-omitted
            :documentation "Advisory values omitted by the caller's budget.")
   (receipts :initarg :receipts :reader context-selection--receipts
             :documentation "Payload-free keys paired with their activation tokens."))
  (:documentation "An unconsumed request assembly. Complete only after successful delivery."))

(defun make-context-resolver ()
  "Create independent receipt state. Session keys are compared with EQUAL."
  (make-instance 'context-resolver))

(defun context-resolver-reset (resolver &key (session-key nil session-key-p))
  "Forget receipt and activation state, optionally for only SESSION-KEY."
  (bordeaux-threads:with-lock-held ((context-resolver--lock resolver))
    (dolist (table (list (context-resolver--activations resolver)
                        (context-resolver--delivered resolver)))
      (if session-key-p
          (let ((keys nil))
            (maphash (lambda (key value)
                       (declare (ignore value))
                       (when (equal session-key (first key))
                         (push key keys)))
                     table)
            (dolist (key keys)
              (remhash key table)))
          (clrhash table))))
  nil)

(defun context--fit-budget (contributions budget cost-function)
  "Return selected and omitted values using required-first stable priority order."
  (let ((mandatory (remove-if-not
                    (lambda (value)
                      (eq (context-contribution-class value) ':mandatory))
                    contributions))
        (advice (stable-sort
                 (remove-if-not
                  (lambda (value)
                    (eq (context-contribution-class value) ':advice))
                  contributions)
                 #'> :key #'context-contribution-priority))
        (selected nil)
        (omitted nil)
        (spent 0))
    (dolist (value advice)
      (let ((cost (funcall cost-function value)))
        (unless (and (realp cost) (<= 0 cost))
          (error 'context-contribution-error
                 :message "Context contribution cost must be a nonnegative real number."))
        (if (<= (+ spent cost) budget)
            (progn
              (incf spent cost)
              (push value selected))
            (push value omitted))))
    (values (append mandatory (nreverse selected)) (nreverse omitted))))

(defun context--eligible (resolver session-key contributions)
  "Return eligible values and detached activation receipts under RESOLVER's lock."
  (let* ((activations (context-resolver--activations resolver))
         (delivered (context-resolver--delivered resolver))
         (keys (loop for value in contributions
                     when (eq (context-contribution-lifetime value) ':next-request)
                       collect (list session-key (context--contribution-key value))))
         (inactive nil))
    (maphash (lambda (key token)
               (declare (ignore token))
               (when (and (equal session-key (first key))
                          (not (member key keys :test #'equal)))
                 (push key inactive)))
             activations)
    (dolist (key inactive)
      (remhash key activations)
      (remhash key delivered))
    (dolist (key keys)
      (unless (gethash key activations)
        (setf (gethash key activations) (gensym "ACTIVATION"))))
    (values
     (remove-if (lambda (value)
                  (and (eq (context-contribution-lifetime value) ':next-request)
                       (gethash (list session-key (context--contribution-key value))
                                delivered)))
                contributions)
     (mapcar (lambda (key) (cons key (gethash key activations))) keys))))

(defun context-resolve (contributions &key resolver session-key (budget 1500)
                        (cost-function #'context-contribution-token-estimate))
  "Assemble CONTRIBUTIONS with explicit RESOLVER state and advisory BUDGET.

Deduplicate before supersession and conflict resolution. Mandatory contributions
win ties with advice and bypass the advisory budget. Stable priority order breaks
budget competition. Instruction and evidence are retained as separate values;
rendering and prompt policy belong to the caller. No receipt is consumed here.

The caller must supply a finite proper list and keep session keys immutable.
Other lifetimes follow caller relevance; only :NEXT-REQUEST creates receipts."
  (check-type resolver context-resolver)
  (unless (and (realp budget) (<= 0 budget))
    (error 'context-contribution-error :message "Context budget must be nonnegative."))
  (unless (handler-case
              (and (integerp (list-length contributions))
                   (every (lambda (value) (typep value 'context-contribution))
                          contributions))
            (type-error () nil))
    (error 'context-contribution-error
           :message "Context assembly requires a finite list of contributions."))
  (let ((resolved (context--resolve-conflicts
                   (context--apply-supersession
                    (context--deduplicate contributions)))))
    (multiple-value-bind (eligible receipts)
        (bordeaux-threads:with-lock-held ((context-resolver--lock resolver))
          (context--eligible resolver session-key resolved))
      ;; Cost functions are caller code and must not run under the state lock.
      (multiple-value-bind (selected omitted)
          (context--fit-budget eligible budget cost-function)
        (make-instance
         'context-selection
         :resolver resolver :contributions selected :omitted omitted
         :receipts (remove-if-not
                    (lambda (receipt)
                      (find (second (first receipt)) selected :test #'equal
                            :key #'context--contribution-key))
                    receipts))))))

(defun context-selection-complete (selection)
  "Consume selected next-request activations after successful request delivery.

Repeated completion is harmless. A delayed completion cannot consume a later
activation, a reset session, or another resolver's state."
  (let ((resolver (context-selection--resolver selection)))
    (bordeaux-threads:with-lock-held ((context-resolver--lock resolver))
      (dolist (receipt (context-selection--receipts selection))
        (when (eq (rest receipt)
                  (gethash (first receipt) (context-resolver--activations resolver)))
          (setf (gethash (first receipt) (context-resolver--delivered resolver))
                (rest receipt))))))
  nil)

