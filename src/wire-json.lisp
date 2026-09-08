(in-package #:cl-llm-provider-api)


(deftype json-object () 'hash-table)

(deftype non-empty-string () '(and string (satisfies non-empty-string-p)))

(defun non-empty-string-p (value)
  "Return true for a nonempty string."
  (and (stringp value) (plusp (length value))))

(defun json-object-p (value)
  "Return true for the wire JSON object representation."
  (hash-table-p value))

(defun bounded-string (value &key (limit 8000))
  "Return bounded printable wire diagnostics."
  (let ((text
         (if (stringp value)
             value
             (princ-to-string value))))
    (if (> (length text) limit)
        (subseq text 0 limit)
        text)))

(defparameter *json-decoded-false*
  ':json-false
  "The portable internal marker preserving decoded JSON false across encoding.")

(defun json-object (&rest key-values)
  "Return a string-keyed JSON object built from alternating KEY-VALUES."
  (unless (evenp (length key-values))
    (error 'provider-api-error :message
           "JSON objects require an even number of key and value arguments."))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (unless (stringp key)
               (error 'provider-api-error :message
                      (format nil "JSON object key ~S is not a string." key))) (setf (gethash
                                                                                      key
                                                                                      object)
                                                                                       value))
    object))

(defun json-array (&rest elements)
  "Return a JSON array containing ELEMENTS."
  (coerce elements 'vector))

(defun json-object-copy (object)
  "Return a detached shallow copy of JSON OBJECT."
  (let ((copy
         (make-hash-table :test (hash-table-test object) :size
                          (max 1 (hash-table-count object)))))
    (maphash (lambda (key value) (setf (gethash key copy) value)) object)
    copy))

(defun json-get-present (object key)
  "Return KEY from JSON OBJECT and whether the key is present.

Decoded JSON false is presented as NIL while its internal marker remains in
OBJECT so that re-encoding preserves the distinction from JSON null."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (values
     (if (eq value *json-decoded-false*)
         nil
         value)
     present-p)))

(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when the key is absent.

Decoded JSON false is presented as NIL while its internal marker remains in
OBJECT so that re-encoding preserves the distinction from JSON null."
  (multiple-value-bind (value present-p)
      (json-get-present object key)
    (if present-p
        value
        default)))

(defun json-string= (value expected)
  "Return true when VALUE is the JSON string EXPECTED."
  (and (stringp value) (string= value expected)))

(defun json-string-member-p (value expected)
  "Return true when VALUE is a JSON string in EXPECTED."
  (not (null (and (stringp value) (member value expected :test #'string=)))))

(defun json--encoding-value (value)
  "Return VALUE with decoded-false markers translated for Yason encoding."
  (cond ((eq value *json-decoded-false*) yason:false)
        ((json-object-p value)
         (let ((copy
                (make-hash-table :test (hash-table-test value) :size
                                 (max 1 (hash-table-count value)))))
           (maphash
            (lambda (key child) (setf (gethash key copy) (json--encoding-value child)))
            value)
           copy))
        ((stringp value) value)
        ((vectorp value) (map 'vector #'json--encoding-value value))
        ((consp value) (mapcar #'json--encoding-value value)) (t value)))

(defun json-encode (value)
  "Encode VALUE as a compact JSON string."
  (with-output-to-string (stream) (yason:encode (json--encoding-value value) stream)))

(defun json-decode (source)
  "Decode one JSON value from SOURCE without conflating false and null."
  (let ((yason:*parse-json-arrays-as-vectors* t)
        (yason:*parse-json-booleans-as-symbols* t)
        (yason:true t)
        (yason:false *json-decoded-false*))
    (yason:parse source)))

(defun json-object-source-p (source)
  "Return true when SOURCE contains exactly one JSON object and whitespace."
  (and (stringp source)
       (handler-case
        (with-input-from-string (stream source)
          (let ((yason:*parse-json-arrays-as-vectors* t)
                (yason:*parse-json-booleans-as-symbols* t)
                (yason:true t)
                (yason:false *json-decoded-false*))
            (let ((value (yason:parse stream)))
              (loop for character = (peek-char nil stream nil nil)
                    while (and character
                               (member character '(#\Space #\Tab #\Newline #\Return)))
                    do (read-char stream))
              (and (json-object-p value) (null (peek-char nil stream nil nil))))))
        (error nil nil))))

(deftype option (type) "An optional wire value." `(or null ,type))

(define-condition configuration-error
    (provider-api-error)
    nil
  (:documentation "Invalid projected wire input."))
