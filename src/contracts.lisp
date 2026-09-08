(in-package #:cl-llm-provider-api)

(define-condition output-contract-error
    (provider-api-error)
    ((field :initarg :field :initform nil :reader output-contract-error-field
      :documentation "The unsupported or malformed native schema field."))
  (:documentation "A native structured-output schema violates the supported subset."))

(define-condition output-value-error
    (provider-api-error)
    nil
  (:documentation "An exact JSON or portable structured-output value is invalid."))

(defparameter *output-contract-types*
  '(:object :array :string :number :integer :boolean :null)
  "Supported native structured-output types, not the complete JSON Schema vocabulary.")

(defparameter *output-contract-property-name-limit*
  32768
  "The maximum character count of a native property name.")

(defun output-contract--error (&key field message)
  "Signal a payload-free diagnostic for an invalid native schema field."
  (error 'output-contract-error :field field :message message))

(defun output--non-empty-string-p (value)
  "Return true for a nonempty string."
  (and (stringp value) (plusp (length value))))

(defun output--json-object ()
  "Create the string-keyed object representation accepted by Yason."
  (make-hash-table :test #'equal))

(defun output--proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (handler-case (or (null value) (and (consp value) (integerp (list-length value))))
                (type-error nil nil)))

(defun output--plist-key-present-p (plist key)
  "Return true when proper PLIST contains KEY in a key position."
  (loop for tail on plist by #'cddr
        thereis (eq (first tail) key)))

(defun output--plist-alist (value allowed-fields)
  "Validate native plist VALUE and return its ordered key-value pairs."
  (unless (output--proper-list-p value)
    (output-contract--error :message "The value must be a proper list."))
  (unless (evenp (length value))
    (output-contract--error :message "The property list has a key without a value."))
  (let ((seen (make-hash-table :test #'eq))
        (pairs nil))
    (loop for (key child) on value by #'cddr
          do (unless (keywordp key)
               (output-contract--error :message "A property key is not a keyword."))
             (unless (member key allowed-fields :test #'eq)
               (output-contract--error
                :field key :message "The property is not part of the native output contract."))
             (when (gethash key seen)
               (output-contract--error
                :field key :message "The property occurs more than once."))
             (setf (gethash key seen) t)
             (push (cons key child) pairs))
    (nreverse pairs)))

(defun output--alist-value (key pairs)
  "Return KEY's value and presence flag from ordered PAIRS."
  (let ((pair (assoc key pairs :test #'eq)))
    (values (and pair (rest pair)) (and pair t))))

(defun output--unique-list-p (values &key (test #'equal))
  "Return true when VALUES has no duplicate elements under TEST."
  (loop for tail on values
        always (not (member (first tail) (rest tail) :test test))))

(defun output--json-number-p (value)
  "Return true when VALUE is a finite JSON-representable number."
  (or (integerp value)
      (and (floatp value)
           #+sbcl
           (not (or (sb-ext:float-nan-p value) (sb-ext:float-infinity-p value)))
           #-sbcl
           (= value value))))

(defun output-json-number-p (value)
  "Return true when VALUE is a finite number representable at a JSON boundary."
  (output--json-number-p value))

(defun output--enum-value-p (value)
  "Return true when VALUE is a scalar supported by native output enum syntax."
  (or (stringp value) (output--json-number-p value) (eq value t) (null value)
      (eq value :null)))

(defun output--value-has-type-p (value type)
  "Return true when native enum VALUE has schema TYPE."
  (case type
    (:string (stringp value))
    (:integer (integerp value))
    (:number (output--json-number-p value))
    (:boolean (or (eq value t) (null value)))
    (:null (eq value :null))
    (otherwise nil)))

(defun output--native-value-equal-p (left right)
  "Return true when native scalar values are equal under JSON semantics."
  (if (and (numberp left) (numberp right))
      (= left right)
      (equal left right)))

(defun output--normalize-enum (value &key type)
  "Validate and copy one native output enum VALUE."
  (unless (and (output--proper-list-p value) value)
    (output-contract--error :field ':enum :message
     "An output enum must be a non-empty proper list."))
  (unless (every #'output--enum-value-p value)
    (output-contract--error :field ':enum :message
     "Output enum values must be strings, JSON numbers, T, NIL, or :NULL."))
  (unless (output--unique-list-p value :test #'output--native-value-equal-p)
    (output-contract--error :field ':enum :message
     "Output enum values must be unique."))
  (when
      (and type
           (not (every (lambda (item) (output--value-has-type-p item type)) value)))
    (output-contract--error :field ':enum :message
     (format nil "An enum value does not have declared type ~S." type)))
  (copy-list value))

(defun output--normalize-properties (value)
  "Validate and normalize a native object property association list."
  (unless (output--proper-list-p value)
    (output-contract--error :field ':properties :message
     "Output properties must be a proper association list."))
  (let ((names nil) (properties nil))
    (dolist (entry value)
      (unless
          (and (output--proper-list-p entry) (= (length entry) 2)
               (output--non-empty-string-p (first entry)))
        (output-contract--error :field ':properties :message
         "Each output property must be a two-element list of name and schema."))
      (when (> (length (first entry)) *output-contract-property-name-limit*)
        (output-contract--error :field ':properties :message
         "An output property name exceeds the string bound."))
      (when (member (first entry) names :test #'string=)
        (output-contract--error :field ':properties :message
         (format nil "Output property ~S occurs more than once." (first entry))))
      (push (first entry) names)
      (push (list (first entry) (output-schema--normalize (second entry))) properties))
    (nreverse properties)))

(defun output-schema--normalize (schema)
  "Validate and return a canonical copy of native output SCHEMA.

The supported syntax is deliberately smaller than JSON Schema. A schema is a
proper plist with :TYPE and optional :ENUM. Object schemas may add
:PROPERTIES, :REQUIRED, and :ADDITIONAL-PROPERTIES. Array schemas require
:ITEMS and may add :MIN-ITEMS and :MAX-ITEMS. An enum-only schema may omit
:TYPE. Native NIL and :NULL denote JSON false and JSON null respectively only
inside enum value positions."
  (let* ((pairs
          (output--plist-alist schema
           '(:type :enum :properties :required :additional-properties :items :min-items
             :max-items)))
         (type nil)
         (type-present-p nil)
         (enum nil)
         (enum-present-p nil))
    (multiple-value-setq (type type-present-p) (output--alist-value :type pairs))
    (multiple-value-setq (enum enum-present-p) (output--alist-value :enum pairs))
    (unless (or type-present-p enum-present-p)
      (output-contract--error :field ':type :message
       "An output schema requires :TYPE or :ENUM."))
    (when (and type-present-p (not (member type *output-contract-types* :test #'eq)))
      (output-contract--error :field ':type :message
       (format nil "Unsupported output type ~S." type)))
    (let* ((allowed
            (case type
              (:object '(:type :enum :properties :required :additional-properties))
              (:array '(:type :enum :items :min-items :max-items))
              ((:string :number :integer :boolean :null) '(:type :enum))
              (otherwise '(:enum))))
           (unknown
            (find-if-not (lambda (pair) (member (first pair) allowed :test #'eq))
                         pairs)))
      (when unknown
        (output-contract--error :field (first unknown) :message
         (format nil "The property is not valid for output type ~S." type))))
    (when (and enum-present-p (member type '(:object :array) :test #'eq))
      (output-contract--error :field ':enum :message
       "Object and array output schemas do not support scalar enums."))
    (let ((normalized (list :type type)))
      (unless type-present-p (setf normalized nil))
      (when enum-present-p
        (setf normalized
                (append normalized
                        (list :enum (output--normalize-enum enum :type type)))))
      (case type
        (:object
         (multiple-value-bind (properties properties-present-p)
             (output--alist-value :properties pairs)
           (let ((normalized-properties
                  (if properties-present-p
                      (output--normalize-properties properties)
                      nil)))
             (when properties-present-p
               (setf normalized
                       (append normalized (list :properties normalized-properties))))
             (multiple-value-bind (required required-present-p)
                 (output--alist-value :required pairs)
               (when required-present-p
                 (unless
                     (and (output--proper-list-p required)
                          (every #'output--non-empty-string-p required)
                          (output--unique-list-p required :test #'string=))
                   (output-contract--error :field ':required :message
                    "Object :REQUIRED must be a proper list of unique non-empty strings."))
                 (unless
                     (every
                      (lambda (name)
                        (assoc name normalized-properties :test #'string=))
                      required)
                   (output-contract--error :field ':required :message
                    "Every required name must have a declared property schema."))
                 (setf normalized
                         (append normalized (list :required (copy-list required))))))
             (multiple-value-bind (additional additional-present-p)
                 (output--alist-value :additional-properties pairs)
               (when additional-present-p
                 (unless (typep additional 'boolean)
                   (output-contract--error :field ':additional-properties :message
                    ":ADDITIONAL-PROPERTIES must be T or NIL."))
                 (setf normalized
                         (append normalized
                                 (list :additional-properties additional))))))))
        (:array
         (multiple-value-bind (items items-present-p)
             (output--alist-value :items pairs)
           (unless items-present-p
             (output-contract--error :field ':items :message
              "An array output schema requires :ITEMS."))
           (setf normalized
                   (append normalized (list :items (output-schema--normalize items)))))
         (multiple-value-bind (minimum minimum-present-p)
             (output--alist-value :min-items pairs)
           (multiple-value-bind (maximum maximum-present-p)
               (output--alist-value :max-items pairs)
             (when (and minimum-present-p (not (typep minimum '(integer 0))))
               (output-contract--error :field ':min-items :message
                ":MIN-ITEMS must be a nonnegative integer."))
             (when (and maximum-present-p (not (typep maximum '(integer 0))))
               (output-contract--error :field ':max-items :message
                ":MAX-ITEMS must be a nonnegative integer."))
             (when (and minimum-present-p maximum-present-p (> minimum maximum))
               (output-contract--error :field ':max-items :message
                ":MAX-ITEMS must not be smaller than :MIN-ITEMS."))
             (when minimum-present-p
               (setf normalized (append normalized (list :min-items minimum))))
             (when maximum-present-p
               (setf normalized (append normalized (list :max-items maximum))))))))
      normalized)))

(defun output--native-json-value (value)
  "Convert one native enum value to its JSON representation."
  (cond ((null value) yason:false) ((eq value :null) :null) (t value)))

(defun output-schema->json (schema)
  "Convert validated native output SCHEMA to provider JSON Schema."
  (let ((object (output--json-object)))
    (loop for (key value) on schema by #'cddr
          do (setf (gethash
                    (case key
                      (:additional-properties "additionalProperties")
                      (:min-items "minItems")
                      (:max-items "maxItems")
                      (otherwise (string-downcase (symbol-name key))))
                    object)
                     (case key
                       (:type (string-downcase (symbol-name value)))
                       (:enum
                        (coerce (mapcar #'output--native-json-value value) 'vector))
                       (:properties
                        (let ((properties (output--json-object)))
                          (dolist (entry value)
                            (setf (gethash (first entry) properties)
                                    (output-schema->json (second entry))))
                          properties))
                       (:required (coerce value 'vector))
                       (:additional-properties
                        (if value
                            t
                            yason:false))
                       (:items (output-schema->json value))
                       (otherwise value))))
    object))

(defun output--candidate-matches-type-p (value type)
  "Return true when provider JSON VALUE has native schema TYPE."
  (case type
    (:object (hash-table-p value))
    (:array (and (vectorp value) (not (stringp value))))
    (:string (stringp value))
    (:integer (integerp value))
    (:number (output--json-number-p value))
    (:boolean (or (eq value t) (eq value yason:false)))
    (:null (eq value :null))
    (otherwise nil)))

(defun output--candidate-enum-value (value)
  "Convert provider JSON VALUE to the corresponding native enum value."
  (cond ((eq value yason:false) nil) ((eq value :null) :null) (t value)))

(defun output-schema-valid-p (value schema)
  "Return true when provider JSON VALUE satisfies validated native SCHEMA."
  (let ((type (getf schema :type)) (enum (getf schema :enum)))
    (and (or (null type) (output--candidate-matches-type-p value type))
         (or (null enum)
             (member (output--candidate-enum-value value) enum :test
                     #'output--native-value-equal-p))
         (case type
           (:object
            (let ((properties (getf schema :properties))
                  (required (getf schema :required))
                  (additional
                   (if (output--plist-key-present-p schema :additional-properties)
                       (getf schema :additional-properties)
                       t)))
              (and (every (lambda (name) (nth-value 1 (gethash name value))) required)
                   (loop for name being the hash-keys of value using (hash-value child)
                         for property = (assoc name properties :test #'string=)
                         always (if property
                                    (output-schema-valid-p child (second property))
                                    additional)))))
           (:array
            (let ((minimum (getf schema :min-items))
                  (maximum (getf schema :max-items))
                  (items (getf schema :items)))
              (and (or (null minimum) (>= (length value) minimum))
                   (or (null maximum) (<= (length value) maximum))
                   (loop for child across value
                         always (output-schema-valid-p child items)))))
           (otherwise t)))))

(defun output-json->sexp (value)
  "Convert validated provider JSON VALUE to a portable tagged native tree."
  (cond
   ((hash-table-p value)
    (cons :object
          (sort
           (loop for key being the hash-keys of value using (hash-value child)
                 collect (list key (output-json->sexp child)))
           #'string< :key #'first)))
   ((stringp value) value)
   ((vectorp value)
    (cons :array
          (loop for child across value
                collect (output-json->sexp child))))
   ((eq value yason:false) nil) ((eq value :null) :null)
   ((or (output--json-number-p value) (eq value t)) value)
   (t
     (error 'output-value-error :message
            "The output contains an unsupported JSON value."))))

(defun output-sexp->json (value)
  "Reconstruct provider JSON from a portable tagged structured-output tree."
  (cond
   ((and (output--proper-list-p value) (consp value) (eq (first value) :object))
    (let ((object (output--json-object)))
      (dolist (entry (rest value))
        (unless
            (and (output--proper-list-p entry) (= (length entry) 2)
                 (stringp (first entry)))
          (error 'output-value-error :message
                 "A tagged object entry is malformed."))
        (when (nth-value 1 (gethash (first entry) object))
          (error 'output-value-error :message
                 "A tagged object contains a duplicate key."))
        (setf (gethash (first entry) object) (output-sexp->json (second entry))))
      object))
   ((and (output--proper-list-p value) (consp value) (eq (first value) :array))
    (coerce (mapcar #'output-sexp->json (rest value)) 'vector))
   ((null value) yason:false) ((eq value :null) :null)
   ((or (stringp value) (output--json-number-p value) (eq value t)) value)
   (t
    (error 'output-value-error :message
            "A tagged output tree contains an unsupported value."))))


(defun output-schema-normalize (schema &key (maximum-nodes 8192) (maximum-depth 128)
                                          (property-name-limit 32768))
  "Validate and copy the native structured-output subset of JSON Schema.

Support :TYPE, scalar :ENUM, object :PROPERTIES/:REQUIRED and boolean
:ADDITIONAL-PROPERTIES, and array :ITEMS/:MIN-ITEMS/:MAX-ITEMS. Native enum
NIL denotes false and :NULL denotes null. Reject unknown fields, malformed
lists, cycles, excessive nesting, and inconsistent bounds."
  (let ((nodes 0)
        (*output-contract-property-name-limit* property-name-limit))
    (labels ((visit (value depth)
               (when (or (> (incf nodes) maximum-nodes) (> depth maximum-depth))
                 (output-contract--error :message "The output schema exceeds its structural bounds."))
               (when (consp value)
                 (unless (output--proper-list-p value)
                   (output-contract--error :message "The output schema requires finite proper lists."))
                 (dolist (child value)
                   (visit child (1+ depth))))))
      (visit schema 0))
    (output-schema--normalize schema)))

(defun output-json-decode (source)
  "Decode exactly one JSON value, distinguishing false from null and empty arrays."
  (handler-case
      (with-input-from-string (stream source)
        (let ((yason:*parse-json-arrays-as-vectors* t)
              (yason:*parse-json-booleans-as-symbols* t)
              (yason:*parse-json-null-as-keyword* t)
              (yason:true t)
              (end (gensym "END")))
          (let ((value (yason:parse stream)))
            (unless (eq (peek-char t stream nil end) end)
              (error 'output-value-error :message "Trailing data follows the JSON value."))
            value)))
    (output-value-error (condition)
      (error condition))
    (error ()
      ;; Parser diagnostics may include input payloads. Do not retain them.
      (error 'output-value-error :message "Could not decode the JSON value."))))

