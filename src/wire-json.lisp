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
  (handler-case
      (apply #'argo:json-object key-values)
    (argo:json-error ()
      (error 'provider-api-error :message "Invalid JSON object arguments."))))

(defun json-array (&rest elements)
  "Return a JSON array containing ELEMENTS."
  (apply #'argo:json-array elements))

(defun json-object-copy (object)
  "Return a detached shallow copy of JSON OBJECT."
  (argo:json-object-copy object))

(defun json-get-present (object key)
  "Return KEY from OBJECT and its presence, presenting the bound false marker as NIL."
  (argo:json-get-present object key :false-value *json-decoded-false*))

(defun json-get (object key &optional default)
  "Return KEY from OBJECT, or DEFAULT when absent, presenting decoded false as NIL."
  (multiple-value-bind (value present-p) (json-get-present object key)
    (if present-p value default)))

(defun json-string= (value expected)
  "Return true when VALUE is the JSON string EXPECTED."
  (argo:json-string= value expected))

(defun json-string-member-p (value expected)
  "Return true when VALUE is a JSON string in EXPECTED."
  (argo:json-string-member-p value expected))

(defun json-encode (value)
  "Encode VALUE as compact JSON, preserving the bound false marker."
  (handler-case
      (argo:json-encode value :false-value *json-decoded-false*)
    (error ()
      (error 'provider-api-error :message "Could not encode the JSON value."))))

(defun json-decode (source)
  "Decode JSON from a string or one value from a character stream.

String input must contain exactly one RFC 8259 value and whitespace.
Streams retain incremental Yason parsing so the following value is unread.
False uses *JSON-DECODED-FALSE*, null NIL, and arrays vectors."
  (handler-case
      (if (streamp source)
          ;; Argo's bounded document codec accepts strings. Keep this adapter
          ;; for callers that read successive values from an open stream.
          (let ((yason:true                    t)
                (yason:false                   *json-decoded-false*)
                (yason:*parse-object-as-alist* nil))
            (yason:parse source :object-as ':hash-table :object-key-fn #'identity
                                :json-arrays-as-vectors t :json-booleans-as-symbols t
                                :json-nulls-as-keyword nil))
          (argo:json-decode source :false-value *json-decoded-false*))
    (error ()
      (error 'provider-api-error :message "Could not decode the JSON value."))))

(defun json-object-source-p (source)
  "Return true when SOURCE contains exactly one JSON object and whitespace."
  (argo:json-object-source-p source))

(deftype option (type) "An optional wire value." `(or null ,type))

(define-condition configuration-error
    (provider-api-error)
    nil
  (:documentation "Invalid projected wire input."))
