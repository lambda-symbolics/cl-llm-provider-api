(in-package #:cl-llm-provider-api)


(defparameter *provider-rate-limit-event-error-codes*
  '("rate_limit_exceeded" "RESOURCE_EXHAUSTED")
  "Structured SSE error codes reporting exhausted provider allowance.")

(defparameter *provider-retryable-event-error-codes*
  '("server_error" "internal_server_error" "server_is_overloaded" "slow_down"
    "rate_limit_exceeded" "DEADLINE_EXCEEDED" "INTERNAL" "RESOURCE_EXHAUSTED"
    "UNAVAILABLE")
  "Structured SSE error codes eligible for bounded retry.")

(defparameter *provider-error-detail-limit*
  1000
  "The characters of a provider failure explanation shown in its message.")

(defparameter *provider-retryable-http-statuses*
  '(500 502 503 504 507)
  "HTTP statuses eligible for bounded provider retry.")

(defparameter *provider-credential-redaction-marker*
  "[PROVIDER CREDENTIAL REDACTED]"
  "The preferred replacement for a credential echoed by a provider response.")

(defvar *provider-active-credential-values*
  nil
  "Exact credential strings available only inside one provider attempt.")

(defvar *provider-active-credential-redaction-marker*
  nil
  "A request-local marker containing none of the active credential values.")

(defun provider--sanitize-wire-string (source)
  "Redact active exact provider credentials from untrusted wire SOURCE."
  (cl-rfc8628:redact-exact-string-values source *provider-active-credential-values*
                                         (or
                                          *provider-active-credential-redaction-marker*
                                          *provider-credential-redaction-marker*)))

(defun provider--sanitize-wire-value (value)
  "Return a detached provider wire VALUE with active credentials redacted.

Compound structure is always freshly consed, while strings holding no
credential are shared unmodified with VALUE."
  (cond ((stringp value) (provider--sanitize-wire-string value))
        ((hash-table-p value)
         (let ((copy
                (make-hash-table :test (hash-table-test value) :size
                                 (hash-table-count value))))
           (maphash
            (lambda (key child)
              (setf (gethash (provider--sanitize-wire-value key) copy)
                      (provider--sanitize-wire-value child)))
            value)
           copy))
        ((vectorp value) (map 'vector #'provider--sanitize-wire-value value))
        ((consp value)
         (cons (provider--sanitize-wire-value (first value))
               (provider--sanitize-wire-value (rest value))))
        (t value)))

(defun response-header (headers name)
  "Return case-insensitive header NAME from Dexador HEADERS."
  (labels ((matching-name-p (candidate)
             (string-equal (string candidate) name)))
    (cond
     ((hash-table-p headers)
      (loop for key being the hash-keys of headers using (hash-value value)
            when (matching-name-p key) return value))
     ((listp headers)
      (let ((pair (find name headers :key #'first :test #'string-equal)))
        (when pair
          (if (consp (rest pair))
              (second pair)
              (rest pair)))))
     (t nil))))

(defun provider--response-request-id (headers)
  "Return a provider request identifier from common response HEADERS."
  (let ((request-id
         (or (response-header headers "x-request-id")
             (response-header headers "request-id"))))
    (and (non-empty-string-p request-id) request-id)))

(defun provider--error-body-detail (body)
  "Return the human-readable explanation carried by an error BODY, if any."
  (when (non-empty-string-p body)
    (let ((message
           (handler-case
            (let ((decoded (json-decode body)))
              (when (json-object-p decoded)
                (let ((error-value (json-get decoded "error")))
                  (or
                   (and (json-object-p error-value)
                        (let ((text (json-get error-value "message")))
                          (and (non-empty-string-p text) text)))
                   (and (non-empty-string-p error-value) error-value)
                   (let ((detail (json-get decoded "detail")))
                     (and (non-empty-string-p detail) detail))))))
            (error nil nil))))
      (bounded-string (or message body) :limit *provider-error-detail-limit*))))

(defun provider--http-error-message (status body)
  "Return a display message for HTTP STATUS including BODY's explanation."
  (let ((detail (provider--error-body-detail body))
        (hint
         (case status
           (404 "The requested resource or model is not being served.")
           (429 "The provider rate limit was reached.")
           (529 "The provider service is overloaded.")
           ((500 502 503 504 507) "The provider service is having trouble.")
           (t nil))))
    (format nil "The provider returned HTTP ~D.~@[ ~A~]~@[~%~A~]" status hint detail)))

(defgeneric provider-retryable-status-p
    (provider status headers)
  (:documentation
   "Return true when PROVIDER reports transient HTTP STATUS and HEADERS."))

(defmethod provider-retryable-status-p
           ((provider model-provider) (status integer) headers)
  "Retry shared service failures, rate limits, and explicit Retry-After responses."
  (declare (ignore provider))
  (or (= status 429)
      (not (null (member status *provider-retryable-http-statuses* :test #'=)))
      (non-empty-string-p (response-header headers "retry-after"))))

(defun provider--signal-http-status-failure (provider status &key headers raw-body)
  "Signal the typed failure represented by HTTP STATUS, HEADERS, and RAW-BODY."
  (let* ((raw-text (provider--error-body-text raw-body))
         (body (and raw-text (provider--sanitize-wire-string raw-text))))
    (if (= status 401)
        (error 'provider-unauthorized :message
               (format nil "The provider rejected the current ~A credentials."
                       (provider-account-label provider))
               :status status :request-id (provider--response-request-id headers)
               :response nil)
        (error
         (if (provider-retryable-status-p provider status headers)
             'provider-retryable-error
             'provider-error)
         :message (provider--http-error-message status body) :status status :request-id
         (provider--response-request-id headers) :response
         (and body (bounded-string body :limit 2000)))))
  nil)

(defun normalize-response-item (item)
  "Remove transient server item identifiers from replayable provider ITEM."
  (remhash "id" item)
  item)

(defmethod provider-normalize-output-item ((provider model-provider) (item hash-table))
  "Remove transient server item identifiers from replayable ITEM."
  (declare (ignore provider))
  (normalize-response-item item))

(defgeneric provider-note-doom-loop-event
    (provider event)
  (:documentation "React to one server-reported doom-loop detection EVENT.

A provider with resample support may signal PROVIDER-RESAMPLE-REQUESTED to
abandon the looping stream; the default reaction ignores the report."))

(defmethod provider-note-doom-loop-event ((provider model-provider) (event hash-table))
  "Ignore doom-loop reports for providers without resample support."
  (declare (ignore provider event))
  nil)

(defun provider--reasoning-summary-key (event)
  "Return EVENT's stable reasoning summary part identity, when available."
  (let ((item-id (json-get event "item_id"))
        (output-index (json-get event "output_index"))
        (summary-index (json-get event "summary_index")))
    (cond ((integerp output-index) (list :output output-index :summary summary-index))
          ((non-empty-string-p item-id) (list :item item-id :summary summary-index))
          (t nil))))

(defun provider--signal-stream-interruption (headers message)
  "Signal a retryable provider stream interruption described by MESSAGE."
  (error 'response-stream-error :message message :status nil :request-id
         (provider--response-request-id headers) :response nil))

(defun provider--signal-stream-protocol-failure (headers message &key response)
  "Signal a terminal provider stream protocol failure described by MESSAGE."
  (error 'provider-protocol-error :message message :status nil :request-id
         (provider--response-request-id headers) :response
         (and response
              (bounded-string (provider--sanitize-wire-string response) :limit
               *provider-error-detail-limit*))))

(defun provider--transport-failure-message (message condition)
  "Append bounded credential-redacted CONDITION detail to transport MESSAGE."
  (let* ((reported-detail (handler-case (format nil "~A" condition) (error nil "")))
         (detail
          (bounded-string (provider--sanitize-wire-string reported-detail) :limit
           *provider-error-detail-limit*))
         (trimmed-detail (string-trim '(#\Space #\Tab #\Newline #\Return) detail)))
    (if (non-empty-string-p trimmed-detail)
        (format nil "~A: ~A"
                (string-right-trim '(#\FULL_STOP #\Space #\Tab #\Newline #\Return)
                                   message)
                trimmed-detail)
        message)))

(defun provider--signal-transport-failure (message &key retryable-p)
  "Signal a credential-redacted provider transport failure described by MESSAGE."
  (error
   (if retryable-p
       'provider-transport-error
       'provider-error)
   :message message :status nil :request-id nil :response nil))

(defun provider--read-sse-data (stream headers)
  "Read one non-empty SSE payload and normalize transport EOF into a provider condition."
  (handler-case
   (loop for data = (read-sse-data stream)
         unless (and (stringp data) (zerop (length data))) return data)
   (provider-error (condition) (error condition))
   (end-of-file nil
    (provider--signal-stream-interruption headers
     "The provider connection closed during an SSE event."))
   (error nil
          (provider--signal-stream-interruption headers
           "The provider stream could not be read."))))

(defun provider--decode-sse-data (data headers)
  "Decode one non-empty SSE DATA payload or signal a terminal protocol failure."
  (handler-case (provider--sanitize-wire-value (json-decode data))
                (error nil
                       (provider--signal-stream-protocol-failure headers
                        "The provider returned a malformed SSE event." :response
                        data))))

(defun provider--event-response (event)
  "Return EVENT's nested response object, when present."
  (let ((response (json-get event "response")))
    (and (json-object-p response) response)))

(defun provider--event-error-object (event)
  "Return the structured error object nested in EVENT, when present."
  (let* ((response (provider--event-response event))
         (response-error (and response (json-get response "error")))
         (event-error (json-get event "error")))
    (cond ((json-object-p response-error) response-error)
          ((json-object-p event-error) event-error)
          ((equal (json-get event "type") "error") event) (t nil))))

(defun provider--event-error-code (error-object)
  "Return ERROR-OBJECT's code, type, or status when it is a non-empty string."
  (when error-object
    (let ((code (json-get error-object "code"))
          (type (json-get error-object "type"))
          (status (json-get error-object "status")))
      (cond ((non-empty-string-p code) code) ((non-empty-string-p type) type)
            ((non-empty-string-p status) status) (t nil)))))

(defun provider--event-response-id (event current-response-id)
  "Return EVENT's response identifier or CURRENT-RESPONSE-ID."
  (let* ((response (provider--event-response event))
         (nested-id (and response (json-get response "id")))
         (event-id (json-get event "response_id")))
    (cond ((non-empty-string-p nested-id) nested-id)
          ((non-empty-string-p event-id) event-id) (t current-response-id))))

(defun provider--event-request-id (event error-object headers)
  "Return the structured or header request identifier for EVENT."
  (let ((error-request-id (and error-object (json-get error-object "request_id")))
        (event-request-id (json-get event "request_id"))
        (header-request-id (provider--response-request-id headers)))
    (cond ((non-empty-string-p error-request-id) error-request-id)
          ((non-empty-string-p event-request-id) event-request-id)
          ((non-empty-string-p header-request-id) header-request-id) (t nil))))

(defun provider--retryable-event-error-code-p (code)
  "Return true when structured provider CODE describes a transient failure."
  (not
   (null
    (and code
         (member code *provider-retryable-event-error-codes* :test #'string-equal)))))

(defun provider-rate-limit-error-p (condition)
  "Return true when CONDITION reports exhausted provider allowance."
  (and (typep condition 'provider-error)
       (let ((code (provider-error-code condition)))
         (or (eql (provider-error-status condition) 429)
             (and code
                  (not
                   (null
                    (member code *provider-rate-limit-event-error-codes* :test
                            #'string-equal))))))
       t))

(defun provider--condition-string (value)
  "Return VALUE as bounded credential-redacted condition text, when present."
  (when value
    (bounded-string (provider--sanitize-wire-string (princ-to-string value)) :limit
     *provider-error-detail-limit*)))

(defun provider--signal-incomplete-terminal
       (reason headers
        &key request-id response-id response (code "response_incomplete"))
  "Signal a terminal incomplete response with sanitized provider identifiers."
  (let ((sanitized-reason (or (provider--condition-string reason) "unknown")))
    (error 'provider-incomplete-response :message
           (format nil "The provider returned an incomplete response (~A)."
                   sanitized-reason)
           :reason sanitized-reason :status nil :code code :request-id
           (provider--condition-string
            (or request-id (provider--response-request-id headers)))
           :response-id (provider--condition-string response-id) :response
           (provider--condition-string response))))

(defparameter *provider-incomplete-terminal-reasons*
  '("max_output_tokens" "content_filter")
  "Recognized Responses protocol reasons for an incomplete terminal event.")

(defun provider--signal-terminal-protocol-error
       (message headers &key code request-id response-id response)
  "Signal a sanitized terminal protocol failure with provider metadata."
  (error 'provider-protocol-error :message message :status nil :code code :request-id
         (provider--condition-string
          (or request-id (provider--response-request-id headers)))
         :response-id (provider--condition-string response-id) :response
         (provider--condition-string response)))

(defun provider--signal-invalid-terminal-reason
       (reason headers &key request-id response-id response)
  "Signal a terminal protocol failure for an unknown or invalid finish reason."
  (let ((sanitized-reason (or (provider--condition-string reason) "missing")))
    (provider--signal-terminal-protocol-error
     (format nil "The provider returned an invalid terminal reason: ~A."
             sanitized-reason)
     headers :code "invalid_finish_reason" :request-id request-id :response-id
     response-id :response response)))

(defun provider--event-incomplete-reason (event)
  "Return EVENT's response.incomplete reason when it is a non-empty string."
  (let* ((response (json-get event "response"))
         (details
          (and (json-object-p response) (json-get response "incomplete_details")))
         (reason (and (json-object-p details) (json-get details "reason"))))
    (and (non-empty-string-p reason) reason)))

(defun provider--signal-incomplete-response (event &key data headers response-id)
  "Validate terminal EVENT and signal its recognized incomplete reason."
  (let* ((response (json-get event "response"))
         (event-response-id (provider--event-response-id event response-id))
         (request-id (provider--event-request-id event nil headers)))
    (unless (json-object-p response)
      (provider--signal-terminal-protocol-error
       "The provider returned response.incomplete without a response object." headers
       :code "invalid_terminal_response" :request-id request-id :response-id
       event-response-id :response data))
    (let ((reason (provider--event-incomplete-reason event)))
      (unless
          (and reason
               (member reason *provider-incomplete-terminal-reasons* :test #'string=))
        (provider--signal-invalid-terminal-reason reason headers :request-id request-id
         :response-id event-response-id :response data))
      (provider--signal-incomplete-terminal reason headers :request-id request-id
       :response-id event-response-id :response data))))

(defun provider--signal-event-failure (event &key type data headers response-id)
  "Signal EVENT as a structured terminal or retryable provider failure."
  (let* ((error-object (provider--event-error-object event))
         (code (provider--condition-string (provider--event-error-code error-object)))
         (status-value (and error-object (json-get error-object "code")))
         (status (and (integerp status-value) status-value))
         (detail
          (provider--condition-string
           (and error-object (json-get error-object "message"))))
         (message
          (if (non-empty-string-p detail)
              (format nil "The provider returned ~A.~%~A" (or code type)
                      (bounded-string detail :limit 1000))
              (format nil "The provider ended with ~A." type)))
         (condition-type
          (if (provider--retryable-event-error-code-p code)
              'provider-retryable-error
              'provider-error)))
    (error condition-type :message message :status status :code code :request-id
           (provider--condition-string
            (provider--event-request-id event error-object headers))
           :response-id
           (provider--condition-string (provider--event-response-id event response-id))
           :response (provider--condition-string data))))

(defmethod provider-consume-stream
           ((provider model-provider) stream headers event-callback)
  "Consume a Responses protocol STREAM into a provider result."
  (let ((output-items nil)
        (response-id nil)
        (usage nil)
        (turn-completion :unspecified)
        (reasoning-summary-key nil)
        (completed-p nil))
    (loop until completed-p
          for data = (provider--read-sse-data stream headers)
          do (cond
              ((eq data *sse-end-of-stream*)
               (provider--signal-stream-interruption headers
                "The provider stream closed before a terminal event."))
              ((string= data "[DONE]")
               (provider--signal-stream-interruption headers
                "The provider stream ended before a terminal event."))
              (t
               (let* ((event (provider--decode-sse-data data headers))
                      (type (and (json-object-p event) (json-get event "type"))))
                 (cond
                  ((json-string= type "response.created")
                   (let ((response (json-get event "response")))
                     (when (json-object-p response)
                       (setf response-id (json-get response "id"))))
                   (funcall event-callback (make-instance 'provider-progress-event)))
                  ((json-string= type "response.output_text.delta")
                   (funcall event-callback
                            (make-instance 'assistant-delta-event :text
                                           (or (json-get event "delta") ""))))
                  ((json-string= type "response.reasoning_summary_text.delta")
                   (let* ((delta (or (json-get event "delta") ""))
                          (next-key
                           (and (plusp (length delta))
                                (provider--reasoning-summary-key event)))
                          (new-part-p
                           (and next-key reasoning-summary-key
                                (not (equal next-key reasoning-summary-key)))))
                     (when next-key (setf reasoning-summary-key next-key))
                     (funcall event-callback
                              (make-instance 'reasoning-delta-event :text
                                             (if new-part-p
                                                 (format nil "~2%~A" delta)
                                                 delta)))))
                  ((json-string= type "response.output_item.done")
                   (let ((item (json-get event "item")))
                     (when (json-object-p item)
                       (provider-normalize-output-item provider item)
                       (push item output-items)
                       (funcall event-callback
                                (make-instance 'provider-item-event :item item)))))
                  ((json-string= type "response.completed")
                   (let ((response (json-get event "response")))
                     (unless (json-object-p response)
                       (provider--signal-terminal-protocol-error
                        "The provider returned response.completed without a response object."
                        headers :code "invalid_terminal_response" :request-id
                        (provider--event-request-id event nil headers) :response-id
                        response-id :response data))
                     (when (provider--event-error-object event)
                       (provider--signal-event-failure event :type type :data data
                        :headers headers :response-id response-id))
                     (setf response-id (or (json-get response "id") response-id)
                           usage (json-get response "usage"))
                     (multiple-value-bind (end-turn present-p)
                         (json-get-present response "end_turn")
                       (when present-p
                         (setf turn-completion
                                 (if end-turn
                                     :end
                                     :continue))))
                     (setf completed-p t)
                     (funcall event-callback
                              (make-instance 'provider-completed-event :response-id
                                             response-id :usage usage :turn-completion
                                             turn-completion))))
                  ((json-string= type "response.doom_loop_check")
                   (provider-note-doom-loop-event provider event)
                   (funcall event-callback (make-instance 'provider-progress-event)))
                  ((json-string= type "response.incomplete")
                   (provider--signal-incomplete-response event :data data :headers
                    headers :response-id response-id))
                  ((json-string-member-p type '("response.failed" "error"))
                   (provider--signal-event-failure event :type type :data data :headers
                    headers :response-id response-id))
                  (t
                   (funcall event-callback
                            (make-instance 'provider-progress-event))))))))
    (let* ((ordered-items (nreverse output-items))
           (tool-calls
            (remove-if-not #'clinker-transcript:function-call-item-p ordered-items)))
      (make-instance 'provider-result :response-id response-id :output-items
                     ordered-items :tool-calls tool-calls :usage usage :turn-state
                     (response-header headers "x-codex-turn-state") :turn-completion
                     turn-completion))))
