(in-package #:cl-llm-provider-api)


(defparameter *provider-wire-function-name-maximum-length*
  64
  "Maximum function name length accepted by the shared provider wire codec.")

(defun provider-wire-function-name--literal-character-p (character)
  "Return true when CHARACTER can ride literally in one encoded component."
  (or
   (and (char<= #\LATIN_SMALL_LETTER_A character)
        (char<= character #\LATIN_SMALL_LETTER_Z))
   (and (char<= #\LATIN_CAPITAL_LETTER_A character)
        (char<= character #\LATIN_CAPITAL_LETTER_Z))
   (and (char<= #\DIGIT_ZERO character) (char<= character #\DIGIT_NINE))
   (not (null (find character "_-")))))

(defun provider-wire-function-name--valid-p (name)
  "Return true when NAME obeys the standard provider function-name grammar."
  (and (stringp name) (plusp (length name))
       (<= (length name) *provider-wire-function-name-maximum-length*)
       (every #'provider-wire-function-name--literal-character-p name) t))

(defun provider-wire-function-name--escape-sequence-p (text index)
  "Return true when TEXT has one complete _xHHHHHH_ escape at INDEX."
  (and (<= (+ index 9) (length text)) (char= (char text index) #\LOW_LINE)
       (char= (char text (1+ index)) #\LATIN_SMALL_LETTER_X)
       (loop for position from (+ index 2) below (+ index 8)
             always (digit-char-p (char text position) 16))
       (char= (char text (+ index 8)) #\LOW_LINE) t))

(defun provider-wire-function-name--encode-component (component)
  "Escape non-grammar characters while retaining readable component text."
  (with-output-to-string (output)
    (loop with index = 0
          while (< index (length component))
          for character = (char component index)
          do (cond
              ((provider-wire-function-name--escape-sequence-p component index)
               (write-string "_x00005F_" output) (incf index))
              ((provider-wire-function-name--literal-character-p character)
               (write-char character output) (incf index))
              (t
               (let ((code (char-code character)))
                 (unless (<= code 16777215)
                   (error 'configuration-error :message
                          "A provider tool name contains an unsupported character."))
                 (format output "_x~6,'0X_" code)
                 (incf index)))))))

(defun provider-wire-function-name--decode-component (encoded)
  "Decode one readable wire-name component, or return NIL when malformed."
  (handler-case
   (with-output-to-string (output)
     (loop with index = 0
           while (< index (length encoded))
           for character = (char encoded index)
           do (cond
               ((provider-wire-function-name--escape-sequence-p encoded index)
                (let* ((code
                        (parse-integer encoded :start (+ index 2) :end (+ index 8)
                                       :radix 16 :junk-allowed nil))
                       (decoded (code-char code)))
                  (unless decoded
                    (return-from provider-wire-function-name--decode-component nil))
                  (write-char decoded output)
                  (incf index 9)))
               ((provider-wire-function-name--literal-character-p character)
                (write-char character output) (incf index))
               (t (return-from provider-wire-function-name--decode-component nil)))))
   (error nil nil)))

(defun provider-wire-function-name--encode (namespace name)
  "Encode NAMESPACE and NAME as one readable reversible function name."
  (unless (and (non-empty-string-p namespace) (non-empty-string-p name))
    (error 'configuration-error :message
           "Provider tool namespaces and names must be nonempty strings."))
  (let* ((encoded-namespace (provider-wire-function-name--encode-component namespace))
         (encoded-name (provider-wire-function-name--encode-component name))
         (wire-name
          (format nil "t_~D_~A__~A" (length encoded-namespace) encoded-namespace
                  encoded-name)))
    (unless (provider-wire-function-name--valid-p wire-name)
      (error 'configuration-error :message
             (format nil
                     "Encoded tool name ~A.~A is ~D characters; the provider limit is ~D."
                     namespace name (length wire-name)
                     *provider-wire-function-name-maximum-length*)))
    wire-name))

(defun provider-wire-function-name--decode-readable (wire-name)
  "Decode one length-prefixed readable Autolith function WIRE-NAME."
  (handler-case
   (let* ((length-end
           (and (uiop/utility:string-prefix-p "t_" wire-name)
                (position #\LOW_LINE wire-name :start 2)))
          (encoded-namespace-length
           (and length-end (> length-end 2)
                (parse-integer wire-name :start 2 :end length-end :junk-allowed nil)))
          (namespace-start (and length-end (1+ length-end)))
          (namespace-end
           (and namespace-start encoded-namespace-length
                (+ namespace-start encoded-namespace-length))))
     (unless
         (and namespace-end (plusp encoded-namespace-length)
              (< (+ namespace-end 2) (length wire-name))
              (char= (char wire-name namespace-end) #\LOW_LINE)
              (char= (char wire-name (1+ namespace-end)) #\LOW_LINE))
       (return-from provider-wire-function-name--decode-readable (values nil nil)))
     (let ((namespace
            (provider-wire-function-name--decode-component
             (subseq wire-name namespace-start namespace-end)))
           (name
            (provider-wire-function-name--decode-component
             (subseq wire-name (+ namespace-end 2)))))
       (if (and (non-empty-string-p namespace) (non-empty-string-p name))
           (values namespace name)
           (values nil nil))))
   (error nil (values nil nil))))

(defun provider-wire-function-name--decode-legacy (wire-name)
  "Decode one legacy Base64 Autolith provider function WIRE-NAME."
  (handler-case
   (let* ((decoded
           (cl-base64:base64-string-to-string
            (cl-rfc8628:padded-base64url (subseq wire-name 1)) :uri t))
          (separator (position (code-char 0) decoded)))
     (if (and separator (plusp separator) (< (1+ separator) (length decoded)))
         (values (subseq decoded 0 separator) (subseq decoded (1+ separator)))
         (values nil nil)))
   (error nil (values nil nil))))

(defun provider-wire-function-name--decode (wire-name)
  "Decode a readable or legacy Autolith provider function WIRE-NAME."
  (if (provider-wire-function-name--valid-p wire-name)
      (cond
       ((uiop/utility:string-prefix-p "t_" wire-name)
        (provider-wire-function-name--decode-readable wire-name))
       ((char= (char wire-name 0) #\LATIN_SMALL_LETTER_A)
        (provider-wire-function-name--decode-legacy wire-name))
       (t (values nil nil)))
      (values nil nil)))

(defmethod provider-wire-tool
           ((provider responses-api-provider) (namespace string) (tool hash-table))
  "Encode one namespaced tool as a standard Responses function tool."
  (json-object "type" "function" "name"
   (provider-wire-tool-name provider namespace (json-get tool "name")) "description"
   (json-get tool "description") "strict" yason:false "parameters"
   (json-get tool "parameters")))

(defmethod provider-wire-tools
           ((provider responses-api-provider) (tool-namespaces vector))
  "Flatten namespaced tools while retaining hosted tool declarations."
  (coerce
   (loop for entry across tool-namespaces
         if (and (json-object-p entry)
                 (json-string= (json-get entry "type") "namespace")
                 (vectorp (json-get entry "tools"))
                 (non-empty-string-p (json-get entry "name")))
         append (loop for tool across (json-get entry "tools")
                      when (and (json-object-p tool)
                                (non-empty-string-p (json-get tool "name")))
                      collect (provider-wire-tool provider (json-get entry "name")
                                                  tool)) else
         collect entry)
   'vector))

(defmethod provider-wire-input-item ((provider responses-api-provider) item)
  "Flatten a namespaced function-call ITEM for a standard Responses request."
  (if (and (json-object-p item) (clinker-transcript:function-call-item-p item)
           (non-empty-string-p (json-get item "namespace"))
           (non-empty-string-p (json-get item "name")))
      (let ((copy (json-object-copy item)))
        (setf (gethash "name" copy)
                (provider-wire-tool-name provider (json-get item "namespace")
                                         (json-get item "name")))
        (remhash "namespace" copy)
        copy)
      item))

(defmethod provider-normalize-output-item
           ((provider responses-api-provider) (item hash-table))
  "Strip server identifiers and restore flat wire calls to namespaced calls."
  (call-next-method)
  (when (clinker-transcript:function-call-item-p item)
    (let* ((name (json-get item "name"))
           (dot (and (stringp name) (position #\FULL_STOP name))))
      (when (and dot (plusp dot) (< (1+ dot) (length name)))
        (setf (gethash "namespace" item) (subseq name 0 dot)
              (gethash "name" item) (subseq name (1+ dot))))))
  item)
