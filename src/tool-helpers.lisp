(in-package #:cl-llm-provider-api)

;;;; -- Provider Tool Identifiers --

(defparameter *provider-chat-completions-tool-identifier-limit* 23
  "Maximum encoded length of a generated Chat Completions name component.

Two components of this length, including reserved-escape quoting, fit the
64-character wire codec with its namespace length prefix and separators.")

(defparameter *provider-tool-identifier-hash-characters* 10
  "Number of hexadecimal FNV-1a characters retained in identifiers.")

(defparameter *provider-tool-identifier-minimum-limit*
  (+ *provider-tool-identifier-hash-characters* 2)
  "Smallest valid identifier limit, including base and hash separator.")

(define-condition provider-tool-identifier-error (error)
  ((reason :initarg :reason :reader provider-tool-identifier-error-reason
           :documentation "Human-readable explanation of the identifier error."))
  (:documentation "Base condition for invalid or colliding provider identifiers.")
  (:report (lambda (condition stream)
             (write-string (provider-tool-identifier-error-reason condition)
                           stream))))

(define-condition provider-tool-identifier-duplicate-error
    (provider-tool-identifier-error)
  ()
  (:documentation "Signals duplicate raw names in one identifier mapping."))

(define-condition provider-tool-identifier-limit-error
    (provider-tool-identifier-error)
  ()
  (:documentation "Signals an identifier limit outside the supported budget."))

(define-condition provider-tool-identifier-collision-error
    (provider-tool-identifier-error)
  ()
  (:documentation "Signals distinct raw names producing one provider identifier."))

(defun provider-tool-identifier--invalid (class reason)
  (error class :reason reason))

(defun provider-tool-identifier--hash (string)
  "Return STRING's fixed 64-bit FNV-1a hexadecimal identity."
  (let ((hash #xcbf29ce484222325))
    (loop for octet across (babel:string-to-octets string :encoding :utf-8)
          do (setf hash (mod (* (logxor hash octet) #x100000001b3)
                             #x10000000000000000)))
    (format nil "~16,'0X" hash)))

(defun provider-tool-identifier--safe-prefix-p (prefix)
  "Return true when PREFIX preserves the provider's ASCII identifier grammar."
  (and (stringp prefix)
       (or (zerop (length prefix))
           (and (or (alpha-char-p (char prefix 0))
                    (char= (char prefix 0) #\_))
                (every (lambda (character)
                         (and (< (char-code character) 128)
                              (or (alphanumericp character)
                                  (member character '(#\_ #\-) :test #'char=))))
                       prefix)))))

(defun provider-tool-identifier--base (raw prefix limit)
  "Return a bounded provider-safe base for RAW after PREFIX."
  (let* ((normalized
           (with-output-to-string (stream)
             (loop with previous-underscore-p = nil
                   for character across raw
                   for lower = (char-downcase character)
                   for safe = (if (or (and (<= (char-code lower) 127)
                                            (alphanumericp lower))
                                      (member lower '(#\_ #\-) :test #'char=))
                                  lower #\_)
                   do (unless (and previous-underscore-p (char= safe #\_))
                        (write-char safe stream))
                      (setf previous-underscore-p (char= safe #\_)))))
         (usable (if (plusp (length normalized)) normalized "unnamed"))
         (initial (if (or (alpha-char-p (char usable 0))
                          (char= (char usable 0) #\_))
                        usable
                        (concatenate 'string "tool_" usable)))
         (combined (concatenate 'string prefix initial)))
    (subseq combined 0 (min limit (length combined)))))

(defun provider-tool-identifier-map
    (raw-names &key (prefix "")
                        (limit *provider-chat-completions-tool-identifier-limit*)
                        identity-scope)
  "Map distinct RAW-NAMES to stable provider-safe identifiers.

RAW-NAMES are sanitized for provider grammar. PREFIX must already be a valid
provider identifier prefix. IDENTITY-SCOPE participates in the UTF-8 FNV-1a
input and is useful for independent namespaces."
  (unless (listp raw-names)
    (provider-tool-identifier--invalid
     'provider-tool-identifier-error "Raw tool names must be a list."))
  (unless (every #'stringp raw-names)
    (provider-tool-identifier--invalid
     'provider-tool-identifier-error "Raw tool names must all be strings."))
  (unless (stringp prefix)
    (provider-tool-identifier--invalid
     'provider-tool-identifier-error "Identifier prefix must be a string."))
  (unless (provider-tool-identifier--safe-prefix-p prefix)
    (provider-tool-identifier--invalid
     'provider-tool-identifier-error
     "Identifier prefix contains characters outside provider-safe grammar."))
  (unless (or (null identity-scope) (stringp identity-scope))
    (provider-tool-identifier--invalid
     'provider-tool-identifier-error "Identity scope must be a string or NIL."))
  (unless (and (integerp limit)
               (<= *provider-tool-identifier-minimum-limit* limit)
               (<= limit *provider-chat-completions-tool-identifier-limit*))
    (provider-tool-identifier--invalid
     'provider-tool-identifier-limit-error
     (format nil "Identifier limit must be an integer from ~D through ~D."
             *provider-tool-identifier-minimum-limit*
             *provider-chat-completions-tool-identifier-limit*)))
  (when (/= (length raw-names)
            (length (remove-duplicates raw-names :test #'string=)))
    (provider-tool-identifier--invalid
     'provider-tool-identifier-duplicate-error
     "Provider tool identifiers require distinct raw names."))
  (let ((used (make-hash-table :test #'equal))
        (result (make-hash-table :test #'equal)))
    (dolist (raw raw-names result)
      (let* ((hash-source (if identity-scope
                             (format nil "~D:~A~A" (length identity-scope)
                                     identity-scope raw)
                             raw))
             (suffix (format nil "_~A"
                             (subseq (provider-tool-identifier--hash hash-source)
                                     0 *provider-tool-identifier-hash-characters*)))
             (candidate
               (loop for base-limit from (- limit (length suffix)) downto 1
                     for base = (provider-tool-identifier--base raw prefix base-limit)
                     for identifier = (concatenate 'string base suffix)
                     when (<= (length (provider-wire-function-name--encode-component
                                       identifier))
                              limit)
                       return identifier)))
        (when (gethash candidate used)
          (provider-tool-identifier--invalid
           'provider-tool-identifier-collision-error
           "Distinct provider tool names produced the same identifier."))
        (setf (gethash candidate used) t
              (gethash raw result) candidate)))))

;;;; -- JSON Schema Helpers --

(defun provider-object-schema (properties required)
  "Return a closed JSON object schema with PROPERTIES and REQUIRED names."
  (json-object "type" "object" "properties" properties
               "required" (coerce required 'vector)
               "additionalProperties" *json-decoded-false*))

(defun provider-open-object-schema (properties &key required)
  "Return an open JSON object schema with optional REQUIRED names."
  (let ((schema (json-object "type" "object" "properties" properties)))
    (when required
      (setf (gethash "required" schema) (coerce required 'vector)))
    schema))

(defun provider-array-schema (items &optional description)
  "Return an array schema whose elements follow ITEMS.

DESCRIPTION, when non-NIL, is included in the schema."
  (let ((schema (json-object "type" "array" "items" items)))
    (when description
      (setf (gethash "description" schema) description))
    schema))

(defun provider-enum-schema (values &optional description)
  "Return a string enum schema containing VALUES.

VALUES may be a list or vector. DESCRIPTION, when non-NIL, is included."
  (let ((schema (json-object "type" "string" "enum" (coerce values 'vector))))
    (when description
      (setf (gethash "description" schema) description))
    schema))

(defun provider-string-property (description)
  "Return a documented string property schema."
  (json-object "type" "string" "description" description))

(defun provider-integer-property (description)
  "Return a documented integer property schema."
  (json-object "type" "integer" "description" description))

(defun provider-boolean-property (description)
  "Return a documented boolean property schema."
  (json-object "type" "boolean" "description" description))

(defun provider-schema-required-names (schema)
  "Return well-formed top-level required names from SCHEMA.

Strings are rejected even though they are vectors, because a JSON string is
not a required-name array."
  (let ((required (and (hash-table-p schema) (gethash "required" schema))))
    (cond ((and (vectorp required) (not (stringp required))
                (every #'stringp required))
           (coerce required 'list))
          ((and (listp required) (every #'stringp required)) required)
          (t nil))))

(defun provider-schema-required-groups (schema)
  "Return complete alternative required-name groups from SCHEMA."
  (let* ((base (provider-schema-required-names schema))
         (alternatives (and (hash-table-p schema)
                            (or (gethash "oneOf" schema)
                                (gethash "anyOf" schema))))
         (schemas (cond ((and (vectorp alternatives)
                              (not (stringp alternatives)))
                         (coerce alternatives 'list))
                        ((listp alternatives) alternatives)
                        (t nil))))
    (if schemas
        (mapcar (lambda (alternative)
                  (remove-duplicates
                   (append base (provider-schema-required-names alternative))
                   :test #'string=))
                schemas)
        (list base))))

(defun provider-schema-missing-required-names (schema arguments)
  "Return missing required names, or a :ONE-OF diagnostic, for ARGUMENTS.

Presence is determined by hash-table presence, so false and null values count
as supplied. Malformed schemas contribute no requirements."
  (let ((missing (mapcar (lambda (group)
                           (remove-if (lambda (name)
                                        (nth-value 1 (gethash name arguments)))
                                      group))
                         (provider-schema-required-groups schema))))
    (cond ((some #'null missing) nil)
          ((and (rest missing)
                (every (lambda (group) (= (length group) 1)) missing))
           (cons :one-of (mapcar #'first missing)))
          (t (first (sort missing #'< :key #'length))))))

(defun provider-schema-argument-placeholder (schema)
  "Return a conservative JSON placeholder matching property SCHEMA."
  (let ((enumeration (and (hash-table-p schema) (gethash "enum" schema)))
        (type (and (hash-table-p schema) (gethash "type" schema))))
    (cond
      ((and (vectorp enumeration)
            (not (stringp enumeration))
            (plusp (length enumeration)))
       (aref enumeration 0))
      ((equal type "string")
       "")
      ((member type '("integer" "number") :test #'equal)
       0)
      ((equal type "boolean")
       *json-decoded-false*)
      ((equal type "array")
       #())
      ((equal type "object")
       (json-object))
      (t
       nil))))

(defun provider-call-canonical-name (call)
  "Return the dotted or bare canonical name carried by JSON CALL."
  (let ((namespace (gethash "namespace" call))
        (name (or (gethash "name" call) "")))
    (if (and (stringp namespace) (plusp (length namespace)))
        (format nil "~A.~A" namespace name)
        name)))

(defun provider-group-tool-schemas
    (tools &key (canonical-names nil canonical-names-supplied-p) namespace-function
             schema-function name-function description-function)
  "Group TOOL schemas into ordered namespace declarations.

Namespace and tool callback results are used without reordering. NAME-FUNCTION
returns the local name used when CANONICAL-NAMES is supplied. A supplied NIL
CANONICAL-NAMES selects no tools; omitting the keyword selects all tools.
DESCRIPTION-FUNCTION is called once per namespace, after grouping."
  (let ((groups (make-hash-table :test #'equal))
        (order '())
        (filter-p canonical-names-supplied-p))
    (dolist (tool tools)
      (let* ((namespace (funcall namespace-function tool))
             (name (and name-function (funcall name-function tool)))
             (canonical (and name-function
                             (stringp name)
                             (if (and (stringp namespace) (plusp (length namespace)))
                                 (format nil "~A.~A" namespace name)
                                 name))))
        (when (or (not filter-p)
                  (and canonical (member canonical canonical-names :test #'string=)))
          (multiple-value-bind (group present-p) (gethash namespace groups)
            (unless present-p
              (setf group '()
                    (gethash namespace groups) group)
              (push namespace order))
            (setf (gethash namespace groups)
                  (cons (funcall schema-function tool) group))))))
    (coerce
     (mapcar (lambda (namespace)
               (json-object
                "type" "namespace"
                "name" namespace
                "description" (if description-function
                                  (funcall description-function namespace)
                                  "")
                 "tools" (coerce (nreverse (gethash namespace groups)) 'vector)))
             (nreverse order))
     'vector)))
