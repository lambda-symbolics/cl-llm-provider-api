(in-package #:cl-llm-provider-api)


(define-condition provider-protocol-error
    (provider-error)
    nil
  (:documentation "A provider returned a terminal protocol-invalid response."))

(define-condition provider-incomplete-response
    (provider-error)
    ((reason :initarg :reason :reader provider-incomplete-response-reason :type string
      :documentation "The provider's incomplete response reason, or unknown."))
  (:documentation "A terminal provider response that ended incomplete."))

(define-condition provider-transport-error
    (provider-retryable-error)
    nil
  (:documentation "A transient provider connection failure eligible for reconnection."))

(define-condition response-stream-error
    (provider-transport-error)
    nil
  (:documentation "A provider stream ended without a valid terminal event."))

(define-condition response-stream-limit-error
    (provider-stream-limit-error response-stream-error)
    nil
  (:default-initargs :status nil :request-id nil :response nil)
  (:documentation "The shared SSE decoder rejected an oversized provider stream."))

(define-condition provider-unauthorized
    (provider-error)
    nil
  (:documentation "A bounded provider attempt was rejected as unauthorized."))
