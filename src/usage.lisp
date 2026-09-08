(in-package #:cl-llm-provider-api)


(defun provider-usage--field (usage name)
  "Return USAGE's nonnegative integer field NAME, or NIL."
  (let ((value (and (json-object-p usage) (json-get usage name))))
    (and (typep value '(integer 0)) value)))

(defun provider-usage--nested-field (usage object-name field-name)
  "Return USAGE's nonnegative integer OBJECT-NAME FIELD-NAME, or NIL."
  (provider-usage--field (and (json-object-p usage) (json-get usage object-name))
   field-name))

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
             (created
              (or (provider-usage--field usage "cache_creation_input_tokens")
                  (provider-usage--nested-field usage "input_tokens_details"
                   "cache_write_tokens")
                  (provider-usage--nested-field usage "prompt_tokens_details"
                   "cache_write_tokens"))))
        (maphash (lambda (key value) (setf (gethash key normalized) value)) usage)
        (when input (setf (gethash "input_tokens" normalized) input))
        (when output (setf (gethash "output_tokens" normalized) output))
        (when total (setf (gethash "total_tokens" normalized) total))
        (when cached (setf (gethash "cached_input_tokens" normalized) cached))
        (when created
          (setf (gethash "cache_creation_input_tokens" normalized) created))
        normalized)))
