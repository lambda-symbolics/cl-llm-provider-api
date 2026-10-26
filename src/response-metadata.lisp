(in-package #:cl-llm-provider-api)

(defun provider--observe-response-metadata (provider previous callback
                                          &key model finish-reason usage reasoning-context)
  "Emit an immutable accumulated snapshot when response metadata changes.
Omitted fields in later chunks do not erase earlier observations. Call before
terminal validation so callbacks can retain metadata from unsuccessful streams."
  (let* ((next-model (if (non-empty-string-p model)
                         model
                         (provider-response-metadata-model previous)))
         (next-context (if (non-empty-string-p reasoning-context)
                           reasoning-context
                           (provider-response-metadata-reasoning-context previous)))
         (next-finish (if (non-empty-string-p finish-reason)
                          finish-reason
                          (provider-response-metadata-finish-reason previous)))
         (next-cache (or (provider-cache-write-tokens provider usage)
                         (provider-response-metadata-cache-write-tokens previous)))
         (next-usage (or (json-object-p usage)
                         (provider-response-metadata-usage-observed-p previous))))
    (if (and (equal next-model (provider-response-metadata-model previous))
             (equal next-finish (provider-response-metadata-finish-reason previous))
             (eql next-cache (provider-response-metadata-cache-write-tokens previous))
             (eql next-usage (provider-response-metadata-usage-observed-p previous))
             (equal next-context (provider-response-metadata-reasoning-context previous)))
        previous
        (let ((metadata (make-instance 'provider-response-metadata
                                       :model next-model :finish-reason next-finish
                                       :cache-write-tokens next-cache
                                       :usage-observed-p next-usage
                                       :reasoning-context next-context)))
          (funcall callback
                   (make-instance 'provider-response-metadata-event :metadata metadata))
          metadata))))
