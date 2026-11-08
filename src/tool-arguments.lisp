(in-package #:cl-llm-provider-api)

;;;; -- Tool Argument Contracts --

(defstruct (provider-argument-diagnostic
            (:constructor provider-argument-diagnostic--make
                (&key code path names suggestions groups operator)))
  "A payload-free argument problem. Paths and names identify schema fields only."
  code path names suggestions groups operator)

(defun provider-argument--copy (value)
  "Copy JSON containers recursively without changing VALUE."
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (name item)
                  (setf (gethash (copy-seq name) copy) (provider-argument--copy item))) value)
       copy))
    ((stringp value)
     (copy-seq value))
    ((vectorp value)
     (map 'vector #'provider-argument--copy value))
    ((consp value)
     (mapcar #'provider-argument--copy value))
    (t
     value)))

(defun provider-argument--distance (left right)
  "Return case-insensitive Levenshtein distance between two field names."
  (let* ((left  (string-downcase left))
         (right (string-downcase right))
         (row   (make-array (1+ (length right))
                            :initial-contents (loop for i to (length right) collect i))))
    (loop for a across left for i from 1
          do (let ((previous (aref row 0)))
               (setf (aref row 0) i)
               (loop for b across right for j from 1
                     do (let ((above (aref row j)))
                          (setf (aref row j)
                                (min (1+ above) (1+ (aref row (1- j)))
                                     (+ previous (if (char= a b) 0 1))))
                          (setf previous above)))))
    (aref row (length right))))

(defun provider-argument--suggestion (name candidates)
  "Return a unique plausible nearest name, or NIL for ties."
  (let ((best nil) (distance most-positive-fixnum) (tied-p nil))
    (dolist (candidate candidates)
      (let ((current (provider-argument--distance name candidate)))
        (cond ((< current distance)
               (setf best candidate distance current tied-p nil))
              ((= current distance)
               (setf tied-p t)))))
    (when (and best (not tied-p)
               (<= distance (min 3 (max 1 (floor (length name) 3)))))
      best)))

(defun provider-argument--false-p (value)
  "Recognize wire false and the Yason spelling accepted by Lisp schema builders."
  (or (eq value *json-decoded-false*) (eq value yason:false)))

(defun provider-argument--true-p (value)
  "Recognize JSON true spellings without treating arbitrary non-NIL values as true."
  (or (eq value t) (eq value yason:true)))

(defun provider-schema-argument-diagnostic (schema arguments &key allowed-extra-names)
  "Return an unknown-before-missing structural diagnostic, or NIL.

Only explicitly closed objects without patternProperties reject unknown fields.
Requirements check presence, not truth. Alternative groups record their operator;
this check does not claim complete oneOf or anyOf value validation."
  (let* ((properties (gethash "properties" schema))
         (declared   (when (hash-table-p properties)
                       (sort (loop for name being the hash-keys of properties collect name)
                             #'string<)))
         (unknown    (when (and (nth-value 1 (gethash "additionalProperties" schema))
                                (provider-argument--false-p (gethash "additionalProperties" schema))
                                (not (gethash "patternProperties" schema)))
                       (sort (loop for name being the hash-keys of arguments
                                   unless (or (member name declared :test #'equal)
                                              (member name allowed-extra-names :test #'equal))
                                     collect name) #'string<))))
    (when unknown
      (return-from provider-schema-argument-diagnostic
        (provider-argument-diagnostic--make
         :code :unknown-arguments :path (list (first unknown)) :names unknown
         :suggestions (mapcar (lambda (name)
                               (cons name (provider-argument--suggestion name declared)))
                             unknown))))
    (let ((missing (provider-schema--missing-required-groups schema arguments)))
      (unless (some #'null missing)
        (provider-argument-diagnostic--make
         :code     :missing-required-arguments
         :names    (first (stable-sort (copy-list missing) #'< :key #'length))
         :groups   missing
         :operator (cond ((gethash "oneOf" schema)
                          :one-of)
                         ((gethash "anyOf" schema)
                          :any-of)))))))

(defun provider-schema-argument-retry (schema arguments diagnostic)
  "Return a detached diagnostic retry skeleton; never execute or mutate arguments."
  (let ((copy (provider-argument--copy arguments)))
    (case (provider-argument-diagnostic-code diagnostic)
      (:unknown-arguments
       (let ((suggestions (provider-argument-diagnostic-suggestions diagnostic)))
         (dolist (pair suggestions)
           (let ((target (rest pair)))
            (when (and target (nth-value 1 (gethash (first pair) arguments))
                       (not (nth-value 1 (gethash target copy)))
                       (= 1 (count target suggestions :key #'rest :test #'equal)))
               (remhash (first pair) copy)
              (setf (gethash (copy-seq target) copy)
                    (provider-argument--copy (gethash (first pair) arguments))))))))
      (:missing-required-arguments
       (let ((properties (gethash "properties" schema)))
         (dolist (name (provider-argument-diagnostic-names diagnostic))
          (unless (nth-value 1 (gethash name copy))
            (setf (gethash (copy-seq name) copy)
                  (provider-argument--copy
                   (provider-schema-argument-placeholder
                    (and (hash-table-p properties) (gethash name properties))))))))))
    copy))

(defun provider-argument--equal-p (left right)
  "Compare JSON values with case-sensitive strings and numeric equality."
  (cond
    ((and (provider-argument--false-p left) (provider-argument--false-p right))
     t)
    ((and (provider-argument--true-p left) (provider-argument--true-p right))
     t)
    ((and (numberp left) (numberp right))
     (= left right))
    ((or (stringp left) (stringp right))
     (and (stringp left) (stringp right) (string= left right)))
    ((and (hash-table-p left) (hash-table-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (loop for name being the hash-keys of left using (hash-value value)
                always (and (nth-value 1 (gethash name right))
                            (provider-argument--equal-p value (gethash name right))))))
    ((and (vectorp left) (vectorp right))
     (and (= (length left) (length right))
          (every #'provider-argument--equal-p left right)))
    (t
     (eq left right))))

(defun provider-argument--type-p (value type)
  "Return whether VALUE fits TYPE, and whether TYPE is a supported field type.

A supported scalar/array type may be paired with null in a two-element vector."
  (cond
    ((and (vectorp type) (not (stringp type)))
     (if (and (= (length type) 2) (= (count "null" type :test #'equal) 1)
              (every (lambda (name) (member name '("null" "integer" "string" "boolean" "array") :test #'equal)) type))
         (multiple-value-bind (valid-p supported-p)
             (provider-argument--type-p value (find-if (lambda (name) (not (equal name "null"))) type))
           (values (or (null value) valid-p) supported-p))
         (values nil nil)))
    ((equal type "integer")
     (values (integerp value) t))
    ((equal type "string")
     (values (stringp value) t))
    ((equal type "boolean")
     (values (or (provider-argument--true-p value) (provider-argument--false-p value)) t))
    ((equal type "array")
     (values (and (vectorp value) (not (stringp value))) t))
    (t
     (values nil nil))))

(defun provider-argument--contract-diagnostic (schema path)
  "Reject assertions outside the supported field subset before checking a value."
  (labels ((unsupported (name)
             (provider-argument-diagnostic--make :code :unsupported-contract
                                                 :path (append path (list name)))))
    (unless (hash-table-p schema)
      (return-from provider-argument--contract-diagnostic (unsupported "type")))
    (let* ((raw-type (gethash "type" schema))
           (type     (if (stringp raw-type) raw-type
                         (and (vectorp raw-type) (find-if (lambda (name) (not (equal name "null"))) raw-type)))))
      (unless (nth-value 1 (provider-argument--type-p nil raw-type))
        (return-from provider-argument--contract-diagnostic (unsupported "type")))
      (loop for name being the hash-keys of schema using (hash-value value)
            do (cond
                 ((member name '("type" "description" "title" "default") :test #'equal))
                 ((equal name "enum")
                  (unless (and (vectorp value) (not (stringp value)))
                    (return-from provider-argument--contract-diagnostic (unsupported name))))
                 ((member name '("minimum" "maximum") :test #'equal)
                  (unless (and (equal type "integer") (realp value))
                    (return-from provider-argument--contract-diagnostic (unsupported name))))
                 ((member name '("minLength" "maxLength" "minItems" "maxItems") :test #'equal)
                  (unless (and (typep value '(integer 0))
                               (equal type (if (member name '("minLength" "maxLength") :test #'equal)
                                               "string" "array")))
                    (return-from provider-argument--contract-diagnostic (unsupported name))))
                 ((equal name "items")
                  (unless (equal type "array")
                    (return-from provider-argument--contract-diagnostic (unsupported name)))
                  (let ((diagnostic (provider-argument--contract-diagnostic value (append path (list name)))))
                    (when diagnostic (return-from provider-argument--contract-diagnostic diagnostic))))
                 (t
                  (return-from provider-argument--contract-diagnostic (unsupported name)))))
      nil)))

(defun provider-argument--validate (schema value path)
  "Return a payload-free diagnostic for the supported field contract subset."
  (labels ((problem (code &optional (location path))
             (provider-argument-diagnostic--make :code code :path location)))
    (let ((diagnostic (provider-argument--contract-diagnostic schema path)))
      (when diagnostic (return-from provider-argument--validate diagnostic)))
    (multiple-value-bind (valid-p supported-p) (provider-argument--type-p value (gethash "type" schema))
      (unless supported-p (return-from provider-argument--validate (problem :unsupported-contract)))
      (unless valid-p (return-from provider-argument--validate (problem :invalid-type)))
      (multiple-value-bind (enumeration present-p) (gethash "enum" schema)
        (when (and present-p
                   (not (and (vectorp enumeration) (not (stringp enumeration))
                            (some (lambda (candidate) (provider-argument--equal-p value candidate)) enumeration))))
          (return-from provider-argument--validate (problem :invalid-enum))))
      (when (null value) (return-from provider-argument--validate nil))
      (dolist (bound '(("minimum" . >=) ("maximum" . <=)))
        (multiple-value-bind (limit present-p) (gethash (first bound) schema)
          (when (and present-p
                     (not (and (integerp value) (realp limit)
                               (funcall (rest bound) value limit))))
            (return-from provider-argument--validate (problem :out-of-range)))))
      (dolist (bound '(("minItems" . >=) ("maxItems" . <=)
                       ("minLength" . >=) ("maxLength" . <=)))
        (multiple-value-bind (limit present-p) (gethash (first bound) schema)
          (when (and present-p
                     (not (and (vectorp value) (typep limit '(integer 0))
                               (funcall (rest bound) (length value) limit))))
            (return-from provider-argument--validate (problem :invalid-length)))))
      (when (and (vectorp value) (not (stringp value)) (nth-value 1 (gethash "items" schema)))
        (loop for item across value for index from 0
              for diagnostic = (provider-argument--validate
                                (gethash "items" schema) item (append path (list index)))
              when diagnostic do (return-from provider-argument--validate diagnostic)))
      nil)))

(defun provider-schema-argument-value (schema arguments name)
  "Return VALUE, original SUPPLIED-P, and a field diagnostic or NIL.

Only the selected field is checked. Absent defaults are detached and validated;
explicit false and null never select a default. No input container is mutated."
  (multiple-value-bind (value supplied-p) (gethash name arguments)
    (let* ((properties (gethash "properties" schema))
           (field      (and (hash-table-p properties) (gethash name properties))))
      (unless supplied-p
        (multiple-value-bind (default present-p)
            (if (hash-table-p field) (gethash "default" field) (values nil nil))
          (unless present-p
            (return-from provider-schema-argument-value (values nil nil nil)))
          (setf value (provider-argument--copy default))))
      (values value supplied-p (provider-argument--validate field value (list name))))))
