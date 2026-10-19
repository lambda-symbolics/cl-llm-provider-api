(in-package #:cl-llm-provider-api)


(defun provider-usage--field (usage name)
  "Return USAGE's nonnegative integer field NAME, or NIL."
  (let ((value (and (json-object-p usage) (json-get usage name))))
    (and (typep value '(integer 0)) value)))

(defun provider-usage--nested-field (usage object-name field-name)
  "Return USAGE's nonnegative integer OBJECT-NAME FIELD-NAME, or NIL."
  (provider-usage--field (and (json-object-p usage) (json-get usage object-name))
   field-name))

(defun provider-usage--cache-write-field (usage)
  "Return a reported cache-creation counter, preferring positive aliases to zero."
  (let ((counts
          (remove nil
                  (list (provider-usage--field usage "cache_creation_input_tokens")
                        (provider-usage--field usage "cache_write_tokens")
                        (provider-usage--field usage "cache_write_input_tokens")
                        (provider-usage--field usage "cacheWriteInputTokens")
                        (provider-usage--nested-field usage "input_tokens_details" "cache_write_tokens")
                        (provider-usage--nested-field usage "prompt_tokens_details" "cache_write_tokens")))))
    (or (find-if #'plusp counts) (first counts))))

(defgeneric provider-cache-write-tokens (provider usage)
  (:documentation
   "Return the observed nonnegative cache-write count from wire USAGE, or NIL.
An explicit zero is distinct from an omitted counter; a positive alias takes
precedence over zero. Specialize for hosts that supply synthetic counters
rather than measured usage."))

(defmethod provider-cache-write-tokens ((provider model-provider) usage)
  "Read explicitly reported cache-write counters, including zero."
  (declare (ignore provider))
  (provider-usage--cache-write-field usage))

(defun provider-usage-normalize (usage)
  "Return USAGE with portable token and prompt-cache counters when available."
  (if (not (json-object-p usage))
      usage
      (let* ((normalized (json-object))
             (input
              (or (provider-usage--field usage "input_tokens")
                  (provider-usage--field usage "prompt_tokens")))
             (output
              (or (provider-usage--field usage "output_tokens")
                  (provider-usage--field usage "completion_tokens")))
             (total
              (or (provider-usage--field usage "total_tokens")
                  (and (or input output) (+ (or input 0) (or output 0)))))
             (cached
              (or (provider-usage--field usage "cached_input_tokens")
                  (provider-usage--nested-field usage "input_tokens_details"
                   "cached_tokens")
                  (provider-usage--nested-field usage "prompt_tokens_details"
                   "cached_tokens")
                  (provider-usage--field usage "prompt_cache_hit_tokens")
                  (provider-usage--field usage "cache_read_input_tokens")))
             (created (provider-usage--cache-write-field usage)))
        (maphash (lambda (key value) (setf (gethash key normalized) value)) usage)
        (when input (setf (gethash "input_tokens" normalized) input))
        (when output (setf (gethash "output_tokens" normalized) output))
        (when total (setf (gethash "total_tokens" normalized) total))
        (when cached (setf (gethash "cached_input_tokens" normalized) cached))
        (when created
          (setf (gethash "cache_creation_input_tokens" normalized) created))
        normalized)))
