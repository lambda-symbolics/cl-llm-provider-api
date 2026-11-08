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
             (created    (provider-usage--cache-write-field usage)))
        (maphash (lambda (key value) (setf (gethash key normalized) value)) usage)
        (when input (setf (gethash "input_tokens" normalized) input))
        (when output (setf (gethash "output_tokens" normalized) output))
        (when total (setf (gethash "total_tokens" normalized) total))
        (when cached (setf (gethash "cached_input_tokens" normalized) cached))
        (when created
          (setf (gethash "cache_creation_input_tokens" normalized) created))
        normalized)))

(defun provider-usage-snapshot (usage)
  "Return a detached string-keyed alist of reported nonnegative integer counters.

Accept a JSON object or portable ((NAME VALUE) ...) entries. Inspect only a
fixed schema and at most 64 alist cells per lookup, without traversing arbitrary
metadata. Preserve zero and aliases as reported; do not infer missing totals.
NIL denotes no known counters. Integers retain Common Lisp's exact range."
  (labels ((field (object name)
             "Read NAME from a JSON object or bounded portable association list."
             (if (json-object-p object)
                 (json-get object name)
                 (loop for tail = object then (rest tail)
                       for entry = (and (consp tail) (first tail))
                       repeat 64
                       while (consp tail)
                       when (and (consp entry)
                                 (equal name (first entry))
                                 (consp (rest entry))
                                 (null (nthcdr 2 entry)))
                         return (second entry))))

           (project (object schema)
             "Copy only SCHEMA's numeric leaves from OBJECT."
             (loop for (name . nested) in schema
                   for value = (field object name)
                   for retained = (if nested
                                      (project value nested)
                                      (and (typep value '(integer 0)) value))
                   when retained
                     collect (list (copy-seq name) retained))))
    (project usage
             '(("input_tokens")
               ("output_tokens")
               ("total_tokens")
               ("cached_input_tokens")
               ("cache_creation_input_tokens")
               ("reasoning_tokens")
               ("prompt_tokens")
               ("completion_tokens")
               ("cache_read_input_tokens")
               ("prompt_cache_hit_tokens")
               ("prompt_cache_miss_tokens")
               ("input_tokens_details" ("cached_tokens") ("cache_write_tokens")
                                       ("audio_tokens") ("text_tokens") ("image_tokens"))
               ("prompt_tokens_details" ("cached_tokens") ("cache_write_tokens")
                                        ("audio_tokens") ("text_tokens") ("image_tokens"))
               ("output_tokens_details" ("reasoning_tokens") ("audio_tokens")
                                        ("accepted_prediction_tokens")
                                        ("rejected_prediction_tokens") ("text_tokens"))
               ("completion_tokens_details" ("reasoning_tokens") ("audio_tokens")
                                            ("accepted_prediction_tokens")
                                            ("rejected_prediction_tokens") ("text_tokens"))
               ("cache_creation" ("ephemeral_5m_input_tokens")
                                 ("ephemeral_1h_input_tokens"))))))

(defun provider-usage-merge (aggregate usage)
  "Add one USAGE observation to AGGREGATE and return a detached counter snapshot.

Project both arguments before adding matching fields and nested counters.
Every observation contributes, even when identical to a previous observation.
Missing counters contribute nothing; zero is retained. Integer sums are exact."
  (labels ((merge-counters (left right)
             "Merge already projected counter objects without retaining metadata."
             (dolist (entry right left)
               (let* ((name     (first entry))
                      (value    (second entry))
                      (existing (assoc name left :test #'string=)))
                 (if existing
                     (setf (second existing)
                           (let ((old-value (second existing)))
                             (if (and (integerp old-value) (integerp value))
                                 (+ old-value value)
                                 (merge-counters old-value value))))
                     (setf left (append left (list entry))))))))
    (merge-counters (provider-usage-snapshot aggregate)
                    (provider-usage-snapshot usage))))
