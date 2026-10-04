(in-package #:cl-llm-provider-api)

;;;; -- OpenAI-Compatible Model Lists --

;;; Model catalogs answer GET /models with {"data": [{"id": ...}, ...]}. The
;;; context window, when advertised at all, hides under one of several field
;;; names. Decoding returns registry model specifications: a plist with :NAME
;;; and, when advertised, :CONTEXT-WINDOW.

(defparameter *model-list-context-window-fields*
  '("context_length" "contextLength" "max_model_len"
    "max_context_length" "context_window" "n_ctx")
  "JSON field names that may carry a model's context window.")

(define-condition provider-model-list-error (provider-api-error)
  ()
  (:documentation "A model discovery response was not a valid model list."))

(defun model-list-token-count (value)
  "Return VALUE as a positive integer token count, or NIL.

Integers, integral reals and decimal strings are accepted."
  (cond
    ((and (integerp value) (plusp value))
     value)
    ((and (realp value) (plusp value) (= value (truncate value)))
     (truncate value))
    ((stringp value)
     (let ((parsed (ignore-errors (parse-integer value))))
       (and parsed (plusp parsed) parsed)))
    (t
     nil)))

(defun model-list-entry-context-window (entry)
  "Return the context window model list ENTRY advertises, or NIL.

The fields in *MODEL-LIST-CONTEXT-WINDOW-FIELDS* are tried in order, then
OpenRouter's top_provider.context_length."
  (or (loop for field in *model-list-context-window-fields*
            for window = (model-list-token-count (json-get entry field))
            when window
              return window)
      (let ((top-provider (json-get entry "top_provider")))
        (and (json-object-p top-provider)
             (model-list-token-count (json-get top-provider "context_length"))))))

(defun model-list-decode (body &key entry-predicate)
  "Decode model list response BODY into model specifications.

ENTRY-PREDICATE, when given, keeps only the decoded entries it accepts. An
invalid document or an entry without a string id signals
PROVIDER-MODEL-LIST-ERROR."
  (let* ((decoded (handler-case (json-decode body)
                    (error ()
                      (error 'provider-model-list-error
                             :message "The model discovery response was not valid JSON."))))
         (data (and (json-object-p decoded) (json-get decoded "data"))))
    (unless (vectorp data)
      (error 'provider-model-list-error
             :message "The model discovery response did not contain a data array."))
    (loop for entry across data
          for identifier = (and (json-object-p entry) (json-get entry "id"))
          do (unless (non-empty-string-p identifier)
               (error 'provider-model-list-error
                      :message "The model discovery response contained an invalid model entry."))
          when (or (null entry-predicate) (funcall entry-predicate entry))
            collect (let ((window (model-list-entry-context-window entry)))
                      (if window
                          (list :name identifier :context-window window)
                          (list :name identifier))))))

(defun model-spec-name (spec)
  "Return the model identifier of model specification SPEC."
  (etypecase spec
    (string spec)
    (cons (getf spec :name))))

(defun model-spec-rename (spec name)
  "Return a copy of model specification SPEC identified by NAME."
  (etypecase spec
    (string name)
    (cons (let ((copy (copy-list spec)))
            (setf (getf copy :name) name)
            copy))))
