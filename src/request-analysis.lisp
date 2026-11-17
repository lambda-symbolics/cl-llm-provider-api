(in-package #:cl-llm-provider-api)

;;;; -- Encoded Request Analysis --

(defgeneric provider-request-body (provider request)
  (:documentation "Return the token-bearing body of REQUEST, retaining unknown envelopes intact."))

(defgeneric provider-request-history (provider request)
  (:documentation
   "Return REQUEST's encoded history vector and its field name, or two NIL values.
Unknown transports have no assumed history field."))

(defgeneric provider-request-output-paths (provider)
  (:documentation
   "Return supported output-limit paths, with the field used for writing first.
Paths are field-name lists relative to PROVIDER-REQUEST-BODY. NIL means that
remote output enforcement is unknown."))

(defgeneric provider-request-output-limits (provider request)
  (:documentation "Return the positive output limits encoded in REQUEST, without host policy."))

(defgeneric provider-request-with-output-ceiling (provider request ceiling)
  (:documentation
   "Return a request-local copy with CEILING in the supported output field.
Unknown or non-supporting transports retain their original request. Envelope
adapters specialize this operation to copy routing fields and delegate on the body."))

(defgeneric provider-request-context-body (provider request)
  (:documentation
   "Return REQUEST's body for measured-input comparison, without cache or output controls.
Unknown transports retain their complete request rather than discarding fields."))

(defgeneric provider-request-cache-breakpoints (provider request)
  (:documentation
   "Return explicit cache marker paths and lifetimes relative to REQUEST's body.
Unknown transports report no assumed markers."))

(defgeneric provider-request-image-p (provider value)
  (:documentation "Return whether JSON object VALUE represents image input for PROVIDER."))

(defmethod provider-request-body ((provider model-provider) (request hash-table))
  "Keep an unknown request intact."
  (declare (ignore provider))
  request)

(defmethod provider-request-history ((provider model-provider) (request hash-table))
  "Do not infer conversation history from an unknown transport's field names."
  (declare (ignore provider request))
  (values nil nil))

(defmethod provider-request-output-paths ((provider model-provider))
  "Do not claim remote output enforcement for an unknown transport."
  (declare (ignore provider))
  nil)

(defmethod provider-request-output-limits ((provider model-provider) (request hash-table))
  "Read declared and common positive limits without choosing host min/max policy."
  (let* ((body  (provider-request-body provider request))
         (paths (remove-duplicates
                 (append (provider-request-output-paths provider)
                         '(("max_output_tokens") ("max_completion_tokens") ("max_tokens")
                           ("generationConfig" "maxOutputTokens")))
                 :test #'equal)))
    (remove-if-not (lambda (value) (and (integerp value) (plusp value)))
                   (mapcar (lambda (path) (request-analysis--path-value body path)) paths))))

(defmethod provider-request-with-output-ceiling
    ((provider model-provider) (request hash-table) (ceiling integer))
  "Copy the supported output path without changing caller-owned nested settings."
  (let ((path (first (provider-request-output-paths provider))))
    (if (and path (provider-output-ceiling-p provider))
        (request-analysis--with-path request path ceiling)
        request)))

(defmethod provider-request-context-body ((provider model-provider) (request hash-table))
  "Keep every unknown transport field in measured-input comparison."
  (declare (ignore provider))
  request)

(defmethod provider-request-cache-breakpoints ((provider model-provider) (request hash-table))
  "Do not infer cache behavior from unknown fields."
  (declare (ignore provider request))
  nil)

(defmethod provider-request-image-p ((provider model-provider) (value hash-table))
  "Recognize common image forms conservatively when the transport is unknown."
  (declare (ignore provider))
  (let ((type (json-get value "type"))
        (mime (or (json-get value "mimeType") (json-get value "mime_type"))))
    (not (null (or (member type '("image" "input_image" "image_url") :test #'equal)
                   (and (stringp mime) (uiop:string-prefix-p "image/" mime)))))))

(defun provider-request-characters (provider value &key image-characters)
  "Return encoded character weight and non-text uncertainty.
IMAGE-CHARACTERS is the host allowance for an image at a declared wire position.
Count arbitrary nested JSON in full, including image-shaped tool payloads."
  (labels ((walk (value path)
             (typecase value
               (hash-table
                (let ((image-p (provider-request-image-p provider value)))
                  (if (and image-p image-characters (request-analysis--image-location-p provider path))
                      (values image-characters t)
                      (let ((characters 2) (non-text-p image-p))
                        (maphash
                         (lambda (key item)
                           (multiple-value-bind (count uncertain-p) (walk item (cons key path))
                             (incf characters (+ 4 (length (princ-to-string key)) count))
                             (when (or uncertain-p (equal key "encrypted_content"))
                               (setf non-text-p t))))
                         value)
                        (values characters non-text-p)))))
               (string
                (values (+ 2 (length value)) nil))
               (vector
                (let ((characters 2) (non-text-p nil))
                  (loop for item across value for index from 0
                        do (multiple-value-bind (count uncertain-p) (walk item (cons index path))
                             (incf characters (1+ count))
                             (when uncertain-p (setf non-text-p t))))
                  (values characters non-text-p)))
               (t
                (values (length (princ-to-string value)) nil)))))
    (walk value nil)))

(defun request-analysis--path-value (object path)
  "Read a field/index PATH, returning NIL for absent objects or array positions."
  (reduce (lambda (value field)
            (cond
              ((and (integerp field) (vectorp value) (not (stringp value))
                    (<= 0 field) (< field (length value)))
               (aref value field))
              ((and (stringp field) (json-object-p value))
               (json-get value field))
              (t
               nil)))
          path :initial-value object))

(defun request-analysis--with-path (object path value)
  "Copy each object on PATH before installing VALUE."
  (let ((copy (if (json-object-p object) (json-object-copy object) (json-object))))
    (setf (gethash (first path) copy)
          (if (rest path)
              (request-analysis--with-path (json-get copy (first path)) (rest path) value)
              value))
    copy))

(defun request-analysis--history (provider request field)
  "Return the history vector and FIELD when the declared sequence is present."
  (let ((items (json-get (provider-request-body provider request) field)))
    (if (and (vectorp items) (not (stringp items)))
        (values items field)
        (values nil nil))))

(defun request-analysis--context-body (provider request)
  "Copy input while excluding only declared wire cache annotations and output controls."
  (labels ((copy-value (value)
             (typecase value
               (hash-table
                (let ((copy (json-object)))
                  (maphash (lambda (key item) (setf (gethash key copy) (copy-value item))) value)
                  copy))
               (string
                value)
               (vector
                (map 'vector #'copy-value value))
               (t
                value))))
    (let ((body (copy-value (provider-request-body provider request))))
      (dolist (field '("stream" "stream_options" "store" "max_output_tokens"
                       "max_completion_tokens" "max_tokens" "prompt_cache_key"
                       "prompt_cache_retention"))
        (remhash field body))
      (dolist (path (provider-request-output-paths provider))
        (request-analysis--remove-path body path))
      (dolist (marker (provider-request-cache-breakpoints provider request))
        (let ((object (request-analysis--path-value body (first marker))))
          (when (json-object-p object) (remhash "cache_control" object))))
      body)))

(defun request-analysis--cache-breakpoints (body &key history-field system-field request-p tools-p)
  "Read ephemeral controls only at the encoder's declared annotation positions."
  (labels ((marker (object path)
             (when (json-object-p object)
               (let ((control (json-get object "cache_control")))
                 (when (and (json-object-p control) (equal (json-get control "type") "ephemeral"))
                   (list (list path (json-get control "ttl")))))))

           (array-markers (items path)
             (when (and (vectorp items) (not (stringp items)))
               (loop for item across items for index from 0
                     append (marker item (append path (list index)))))))
    (append (when request-p (marker body nil))
            (when tools-p (array-markers (json-get body "tools") '("tools")))
            (when system-field (array-markers (json-get body system-field) (list system-field)))
            (when history-field
              (let ((messages (json-get body history-field)))
                (when (and (vectorp messages) (not (stringp messages)))
                  (loop for message across messages for index from 0
                        when (json-object-p message)
                          append (array-markers (json-get message "content")
                                                (list history-field index "content")))))))))

(defun request-analysis--remove-path (object path)
  "Remove PATH in a private copy, pruning empty parent objects."
  (when (json-object-p object)
    (let ((field (first path)))
      (if (rest path)
          (let ((child (json-get object field)))
            (request-analysis--remove-path child (rest path))
            (when (and (json-object-p child) (zerop (hash-table-count child)))
              (remhash field object)))
          (remhash field object))))
  object)

(defgeneric request-analysis--image-location-p (provider path)
  (:documentation "Return whether reverse PATH identifies an encoder-declared image part."))

(defmethod request-analysis--image-location-p ((provider model-provider) path)
  "Count unknown wire objects in full, including root image-shaped JSON."
  (declare (ignore provider path))
  nil)

(defun request-analysis--content-image-location-p (path history-field part-field)
  "Recognize direct parts, history vectors and encoded history/content array paths."
  (block nil
    (when (or (null path) (and (integerp (first path)) (null (rest path))))
      (return t))
    (let ((tail      path)
          (content-p nil))
      (loop while (and (integerp (first tail)) (equal (second tail) part-field))
            do (setf tail (nthcdr 2 tail) content-p t))
      (and content-p
           (or (null tail)
               (and (integerp (first tail))
                    (or (null (rest tail))
                        (and (equal (second tail) history-field) (null (nthcdr 2 tail))))))
           t))))
