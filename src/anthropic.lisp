(in-package #:cl-llm-provider-api)

;;;; -- Anthropic Messages Wire Protocol --

(defclass anthropic-messages-provider (model-provider)
  ()
  (:documentation "A streaming Anthropic Messages client over projected request data."))

(defmethod provider-output-ceiling-p ((provider anthropic-messages-provider))
  "Anthropic Messages requires and accepts max_tokens."
  (declare (ignore provider))
  t)

(defmethod provider-retryable-status-p
    ((provider anthropic-messages-provider) (status integer) headers)
  "Retry Anthropic's nonstandard overload status in addition to shared statuses."
  (or (call-next-method)
      (= status 529)))

(defun anthropic--wire-tool (namespace tool)
  "Return one namespaced projected TOOL as an Anthropic tool declaration."
  (json-object
   "name" (openai-compatible--wire-tool-name namespace (json-get tool "name"))
   "description" (json-get tool "description")
   "input_schema" (json-get tool "parameters")))

(defun anthropic--wire-tools (tool-namespaces)
  "Flatten projected namespaces into Anthropic's flat tools array."
  (coerce
   (loop for entry across tool-namespaces
         when (and (json-object-p entry)
                   (json-string= (json-get entry "type") "namespace")
                   (non-empty-string-p (json-get entry "name"))
                   (vectorp (json-get entry "tools")))
           append (loop for tool across (json-get entry "tools")
                        when (and (json-object-p tool)
                                  (non-empty-string-p (json-get tool "name")))
                          collect (anthropic--wire-tool
                                   (json-get entry "name")
                                   tool)))
   'vector))

(defun anthropic--image-source (image-url)
  "Return one Anthropic base64 or remote image source for IMAGE-URL."
  (cond
    ((uiop:string-prefix-p "data:" image-url)
     (let* ((metadata-end (position #\, image-url))
            (metadata (and metadata-end
                           (subseq image-url (length "data:") metadata-end)))
            (base64-p (and metadata
                           (uiop:string-suffix-p metadata ";base64")))
            (media-type (and base64-p
                             (subseq metadata 0
                                     (- (length metadata)
                                        (length ";base64")))))
            (data (and metadata-end (subseq image-url (1+ metadata-end)))))
       (when (and base64-p (non-empty-string-p media-type)
                  (non-empty-string-p data))
         (json-object "type" "base64"
                      "media_type" media-type
                      "data" data))))
    ((or (uiop:string-prefix-p "https://" image-url)
         (uiop:string-prefix-p "http://" image-url))
     (json-object "type" "url" "url" image-url))
    (t
     nil)))

(defun anthropic--signal-request-conversion-failure (message)
  "Signal a terminal Anthropic request conversion failure described by MESSAGE."
  (error 'provider-error
         :message message
         :status nil
         :code "unsupported_content"
         :request-id nil
         :response-id nil
         :response nil))

(defun anthropic--content-block (part)
  "Translate one Responses content PART into an Anthropic content block."
  (let ((type (json-get part "type")))
    (cond
      ((and (json-string-member-p
             type '("input_text" "output_text" "text" "refusal"))
            (stringp (json-get part "text")))
       (json-object "type" "text" "text" (json-get part "text")))
      ((and (json-string= type "input_image")
            (non-empty-string-p (json-get part "image_url")))
       (let ((source (anthropic--image-source (json-get part "image_url"))))
         (when source
           (json-object "type" "image" "source" source))))
      (t
       nil))))

(defun anthropic--content-blocks (content)
  "Translate Responses CONTENT and report whether every part was supported."
  (cond
    ((stringp content)
     (values (list (json-object "type" "text" "text" content)) t))
    ((vectorp content)
     (let ((blocks nil)
           (complete-p t))
       (loop for part across content
             for block = (and (json-object-p part)
                              (anthropic--content-block part))
             do (if block
                    (push block blocks)
                    (setf complete-p nil)))
       (values (nreverse blocks) complete-p)))
    (t
     (values nil nil))))

(defun anthropic--tool-use-block (item)
  "Translate one Responses function-call ITEM into an Anthropic tool_use block."
  (let* ((call-id (json-get item "call_id"))
         (tool-name (json-get item "name"))
         (namespace (json-get item "namespace"))
         (arguments (json-get item "arguments"))
         (input
           (and (non-empty-string-p arguments)
                (handler-case (json-decode arguments)
                  (error ()
                    nil)))))
    (unless (and (non-empty-string-p call-id)
                 (non-empty-string-p tool-name)
                 (json-object-p input))
      (anthropic--signal-request-conversion-failure
       "A replayed function call is not valid Anthropic tool use."))
    (json-object
     "type" "tool_use"
     "id" call-id
     "name" (if (non-empty-string-p namespace)
                (openai-compatible--wire-tool-name namespace tool-name)
                tool-name)
     "input" input)))

(defun anthropic--tool-result-block (item)
  "Translate one Responses function-call output ITEM into a tool_result block."
  (let ((call-id (json-get item "call_id"))
        (output (json-get item "output")))
    (unless (non-empty-string-p call-id)
      (anthropic--signal-request-conversion-failure
       "A replayed tool result has no valid Anthropic tool-use identifier."))
    (json-object
     "type" "tool_result"
     "tool_use_id" call-id
     "content"
     (cond
       ((stringp output)
        output)
       ((vectorp output)
        (multiple-value-bind (blocks complete-p)
            (anthropic--content-blocks output)
          (cond
            ((and complete-p blocks)
             (coerce blocks 'vector))
            ((null blocks)
             (bounded-string (json-encode output) :limit 2000))
            (t
             (anthropic--signal-request-conversion-failure
              "A tool result mixes supported and unsupported content.")))))
       (t
        (bounded-string output :limit 2000))))))

(defstruct (anthropic--message-state
            (:constructor anthropic--message-state (role)))
  "Mutable accumulator for one Anthropic message under construction."
  (role "user" :type string :read-only t)
  (blocks nil :type list))

(defun anthropic--message-state-push (state block)
  "Append BLOCK to STATE, merging adjacent text blocks of one origin."
  (push block (anthropic--message-state-blocks state))
  nil)

(defun anthropic--message-state-render (state)
  "Render STATE as one Anthropic message with content blocks in order."
  (json-object "role" (anthropic--message-state-role state)
               "content" (coerce (nreverse (anthropic--message-state-blocks state))
                                 'vector)))

(defun anthropic--input-messages (items)
  "Translate portable Responses ITEMS into alternating Anthropic messages.

Developer and system items retain their transcript position as user content.
Hoisting them into the top-level system field would invalidate the stable
system cache prefix when mid-conversation guidance changes."
  (let ((messages        nil)
        (state           nil)
        (tool-use-states (make-hash-table :test #'equal)))
    (labels ((flush ()
               "Append the pending message, merging with a same-role tail."
               (when (and state (anthropic--message-state-blocks state))
                 (let ((rendered (anthropic--message-state-render state)))
                   (if (and messages
                            (string= (json-get (first messages) "role")
                                     (json-get rendered "role")))
                       (setf (gethash "content" (first messages))
                             (concatenate 'vector
                                          (json-get (first messages) "content")
                                          (json-get rendered "content")))
                       (push rendered messages))))
               (setf state nil))

             (begin (role)
               "Start or continue a message with ROLE, flushing on changes."
               (when (and state
                          (not (string= (anthropic--message-state-role state)
                                        role)))
                 (flush))
               (unless state
                 (setf state (anthropic--message-state role))))

             (append-message (role content)
               "Append translated CONTENT to a message with ROLE."
               (multiple-value-bind (blocks complete-p)
                   (anthropic--content-blocks content)
                 (unless (and complete-p blocks)
                   (anthropic--signal-request-conversion-failure
                    "A replayed message contains unsupported Anthropic content."))
                 (flush)
                 (begin role)
                 (dolist (block blocks)
                   (anthropic--message-state-push state block)))))
      (dolist (item items)
        (when (json-object-p item)
          (let ((type (json-get item "type")))
            (cond
              ((json-string= type "message")
               (let ((role (json-get item "role")))
                 (cond
                  ((json-string-member-p role '("developer" "system"))
                    (append-message "user" (json-get item "content")))
                   ((json-string-member-p role '("user" "assistant"))
                    (append-message role (json-get item "content")))
                   (t
                    nil))))
              ((and (json-string= type "function_call")
                    (clinker-transcript:function-call-item-p item))
               (let* ((block (anthropic--tool-use-block item))
                      (call-id (json-get block "id")))
                 (when (gethash call-id tool-use-states)
                   (anthropic--signal-request-conversion-failure
                    "A replayed function call reuses an Anthropic tool-use identifier."))
                 (setf (gethash call-id tool-use-states) ':pending)
                 (flush)
                 (begin "assistant")
                 (anthropic--message-state-push state block)))
              ((json-string= type "function_call_output")
               (let* ((block (anthropic--tool-result-block item))
                      (call-id (json-get block "tool_use_id")))
                 (unless (eq (gethash call-id tool-use-states) ':pending)
                   (anthropic--signal-request-conversion-failure
                    "A replayed tool result does not match one pending Anthropic tool use."))
                 (setf (gethash call-id tool-use-states) ':completed)
                 (flush)
                 (begin "user")
                 (anthropic--message-state-push state block)))
              (t
               nil)))))
      (flush)
      (nreverse messages))))

;;;; -- Anthropic Requests --

(defparameter *anthropic-maximum-output-tokens* 32000
  "The output token budget requested for Anthropic streaming turns.")

(defun anthropic--cache-control ()
  "Return one explicit ephemeral Anthropic cache breakpoint declaration."
  (json-object "type" "ephemeral"))

(defun anthropic--mark-cache-breakpoint (object)
  "Add an explicit ephemeral cache breakpoint to OBJECT and return it."
  (setf (gethash "cache_control" object) (anthropic--cache-control))
  object)

(defun anthropic--system-blocks (texts &key cache-p)
  "Return Anthropic system blocks for nonempty TEXTS, optionally cache-marked."
  (let ((kept (remove-if-not #'non-empty-string-p texts)))
    (when kept
      (let ((blocks
              (coerce (mapcar (lambda (text)
                                (json-object "type" "text" "text" text))
                              kept)
                      'vector)))
        (when cache-p
          (anthropic--mark-cache-breakpoint
           (aref blocks (1- (length blocks)))))
        blocks))))

(defun anthropic--mark-tool-cache-breakpoint (tools)
  "Mark the final tool declaration as an explicit Anthropic cache breakpoint."
  (when (plusp (length tools))
    (anthropic--mark-cache-breakpoint (aref tools (1- (length tools)))))
  tools)

(defun anthropic--mark-history-cache-breakpoint (messages)
  "Mark the final content block of chronological MESSAGES as cacheable."
  (when messages
    (let ((content (json-get (first (last messages)) "content")))
      (when (and (vectorp content) (plusp (length content)))
        (anthropic--mark-cache-breakpoint
         (aref content (1- (length content)))))))
  messages)

(defun anthropic--append-messages (messages trailing-messages)
  "Append TRAILING-MESSAGES, merging an equal-role boundary when necessary."
  (cond
    ((null trailing-messages)
     messages)
    ((null messages)
     trailing-messages)
    ((json-string= (json-get (first (last messages)) "role")
                   (json-get (first trailing-messages) "role"))
     (let ((message (first (last messages))))
       (setf (gethash "content" message)
             (concatenate 'vector
                          (json-get message "content")
                          (json-get (first trailing-messages) "content")))
       (append messages (rest trailing-messages))))
    (t
     (append messages trailing-messages))))

(defun anthropic--append-user-texts (messages texts)
  "Append nonempty TEXTS as trailing user blocks without disturbing history."
  (let ((blocks
          (mapcar (lambda (text)
                    (json-object "type" "text" "text" text))
                  (remove-if-not #'non-empty-string-p texts))))
    (cond
      ((null blocks)
       messages)
      ((and messages
            (json-string= (json-get (first (last messages)) "role") "user"))
       (let ((message (first (last messages))))
         (setf (gethash "content" message)
               (concatenate 'vector
                            (json-get message "content")
                            (coerce blocks 'vector)))
         messages))
      (t
       (append messages
               (list (json-object "role" "user"
                                  "content" (coerce blocks 'vector))))))))

;;;; -- Anthropic Stream Decoding --

(defstruct (anthropic--block-state
            (:constructor anthropic--block-state (index type)))
  "Mutable accumulator for one streamed Anthropic content block."
  (index 0 :type (integer 0) :read-only t)
  (type "" :type string :read-only t)
  (id nil :type (option string))
  (name nil :type (option string))
  (initial-input nil :type t)
  (text-stream (make-string-output-stream) :type stream :read-only t)
  (json-stream (make-string-output-stream) :type stream :read-only t))

(defun anthropic--request-id (headers)
  "Return Anthropic's request identifier from response HEADERS, if present."
  (provider--response-request-id headers))

(defun anthropic--signal-protocol-failure
    (message &key headers response-id data)
  "Signal a terminal Anthropic stream protocol failure described by MESSAGE."
  (error 'provider-protocol-error
         :message (provider--sanitize-wire-string message)
         :status nil
         :code "invalid_stream"
         :request-id (anthropic--request-id headers)
         :response-id response-id
         :response
         (and data
              (bounded-string (provider--sanitize-wire-string data) :limit 2000))))

(defun anthropic--signal-incomplete-response
    (reason &key headers response-id data)
  "Signal Anthropic's terminal incomplete response REASON."
  (error 'provider-incomplete-response
         :message
         (format nil "The provider returned an incomplete response (~A)." reason)
         :reason reason
         :status nil
         :code "response_incomplete"
         :request-id (anthropic--request-id headers)
         :response-id response-id
         :response
         (and data
              (bounded-string (provider--sanitize-wire-string data) :limit 2000))))

(defun anthropic--event-index (event &key headers response-id data)
  "Return EVENT's validated nonnegative content block index."
  (let ((index (json-get event "index")))
    (unless (typep index '(integer 0))
      (anthropic--signal-protocol-failure
       "The provider returned an invalid content block index."
       :headers headers :response-id response-id :data data))
    index))

(defun anthropic--tool-arguments (state &key headers response-id data)
  "Return STATE's validated JSON-object tool arguments as wire text."
  (let* ((fragments
           (get-output-stream-string
            (anthropic--block-state-json-stream state)))
         (arguments
           (if (non-empty-string-p fragments)
               fragments
               (json-encode
                (or (anthropic--block-state-initial-input state)
                    (json-object)))))
         (valid-p
           (handler-case
               (json-object-p (json-decode arguments))
             (error ()
               nil))))
    (unless valid-p
      (anthropic--signal-protocol-failure
       "The provider returned malformed tool-use arguments."
       :headers headers :response-id response-id :data data))
    arguments))

(defun anthropic--block-item (state &key headers response-id data)
  "Return STATE's completed portable output item, or NIL for empty text."
  (if (string= (anthropic--block-state-type state) "tool_use")
      (let ((item
              (json-object
               "type" "function_call"
               "call_id" (anthropic--block-state-id state)
               "name" (anthropic--block-state-name state)
               "arguments"
               (anthropic--tool-arguments
                state :headers headers :response-id response-id :data data)
               "status" "completed")))
        (multiple-value-bind (namespace name)
            (openai-compatible--decode-wire-tool-name (json-get item "name"))
          (when (and namespace name)
            (setf (gethash "namespace" item) namespace
                  (gethash "name" item) name)))
        item)
      (let ((text
              (get-output-stream-string
               (anthropic--block-state-text-stream state))))
        (when (plusp (length text))
          (json-object
           "type" "message"
           "status" "completed"
           "role" "assistant"
           "content"
           (json-array
            (json-object "type" "output_text"
                         "text" text
                         "annotations" (json-array))))))))

(defun anthropic--ordered-block-items
    (completed-blocks &key headers response-id data)
  "Return COMPLETED-BLOCKS in contiguous Anthropic content index order."
  (let ((indices
          (sort (loop for index being the hash-keys of completed-blocks
                      collect index)
                #'<)))
    (unless (equal indices
                   (loop for index below (length indices) collect index))
      (anthropic--signal-protocol-failure
       "The provider returned noncontiguous content block indices."
       :headers headers :response-id response-id :data data))
    (loop for index in indices
          for item = (gethash index completed-blocks)
          when item collect item)))

(defun anthropic--stop-reason-completion
    (reason &key headers response-id data)
  "Return the portable turn completion for Anthropic stop REASON."
  (cond
    ((member reason '("end_turn" "stop_sequence" "refusal") :test #'string=)
     ':end)
    ((string= reason "tool_use")
     ':continue)
    ((string= reason "pause_turn")
     (anthropic--signal-protocol-failure
      "The provider requested unsupported server-tool continuation (pause_turn)."
      :headers headers :response-id response-id :data data))
    ((member reason '("max_tokens" "model_context_window_exceeded")
             :test #'string=)
     (anthropic--signal-incomplete-response
      reason :headers headers :response-id response-id :data data))
    (t
     (anthropic--signal-protocol-failure
      (format nil "The provider returned an unknown stop reason: ~A." reason)
      :headers headers :response-id response-id :data data))))

(defun anthropic--usage-valid-p (usage required-fields)
  "Return true when USAGE has nonnegative integer REQUIRED-FIELDS."
  (and (json-object-p usage)
       (every
        (lambda (name)
          (multiple-value-bind (value present-p)
              (gethash name usage)
            (and present-p (typep value '(integer 0)))))
        required-fields)))

(defun anthropic--usage-field (usage name)
  "Return the nonnegative integer USAGE field NAME, or NIL."
  (let ((value (and (json-object-p usage) (json-get usage name))))
    (and (typep value '(integer 0)) value)))

(defun anthropic--portable-usage (start-usage delta-usage)
  "Combine Anthropic message_start and message_delta USAGE into portable form."
  (let* ((uncached-input
           (or (anthropic--usage-field start-usage "input_tokens") 0))
         (cache-read
           (anthropic--usage-field start-usage "cache_read_input_tokens"))
         (cache-created
           (anthropic--usage-field start-usage "cache_creation_input_tokens"))
         (input (+ uncached-input (or cache-read 0) (or cache-created 0)))
         (output (or (anthropic--usage-field delta-usage "output_tokens")
                     (anthropic--usage-field start-usage "output_tokens")
                     0))
         (portable
           (json-object "input_tokens" input
                        "output_tokens" output
                        "total_tokens" (+ input output))))
    (when (or cache-read cache-created)
      (setf (gethash "uncached_input_tokens" portable) uncached-input
            (gethash "cache_read_input_tokens" portable) (or cache-read 0)
            (gethash "cache_creation_input_tokens" portable) (or cache-created 0)))
    portable))

(defmethod provider-consume-stream
    ((provider anthropic-messages-provider) stream headers event-callback)
  "Consume PROVIDER's Anthropic SSE stream into a provider result."
  (declare (ignore provider))
  (let ((open-blocks (make-hash-table))
        (seen-blocks (make-hash-table))
        (completed-blocks (make-hash-table))
        (output-items nil)
        (response-id nil)
        (start-usage (json-object))
        (delta-usage (json-object))
        (stop-reason nil)
        (turn-completion :unspecified)
        (started-p nil)
        (message-delta-seen-p nil)
        (completed-p nil))
    (labels ((require-started (data)
               "Reject content events before the stream's message_start event."
               (unless started-p
                 (anthropic--signal-protocol-failure
                  "The provider returned content before message_start."
                  :headers headers :response-id response-id :data data)))

             (reject-late-block-event (data)
               "Reject block events after top-level message deltas begin."
               (when message-delta-seen-p
                 (anthropic--signal-protocol-failure
                  "The provider returned content after message_delta."
                  :headers headers :response-id response-id :data data))))
      (loop until completed-p
            for data = (provider--read-sse-data stream headers)
            do (when (eq data *sse-end-of-stream*)
                 (provider--signal-stream-interruption
                  headers
                  "The provider stream closed before a terminal event."))
               (let ((event (provider--decode-sse-data data headers)))
                 (unless (json-object-p event)
                   (anthropic--signal-protocol-failure
                    "The provider returned a non-object stream event."
                    :headers headers :response-id response-id :data data))
                 (let ((type (json-get event "type")))
                   (unless (non-empty-string-p type)
                     (anthropic--signal-protocol-failure
                      "The provider returned a stream event without a type."
                      :headers headers :response-id response-id :data data))
                   (cond
                     ((string= type "error")
                      (provider--signal-event-failure
                       event :type "error" :data data :headers headers
                       :response-id response-id))
                     ((string= type "message_start")
                      (when started-p
                        (anthropic--signal-protocol-failure
                         "The provider returned duplicate message_start events."
                         :headers headers :response-id response-id :data data))
                      (let ((message (json-get event "message")))
                        (unless (and (json-object-p message)
                                     (non-empty-string-p (json-get message "id"))
                                     (json-string= (json-get message "role")
                                                   "assistant")
                                     (vectorp (json-get message "content"))
                                     (zerop (length (json-get message "content"))))
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid message_start event."
                           :headers headers :response-id response-id :data data))
                        (setf response-id (json-get message "id")
                              started-p t)
                        (multiple-value-bind (usage present-p)
                            (gethash "usage" message)
                          (unless (and present-p
                                       (anthropic--usage-valid-p
                                        usage '("input_tokens" "output_tokens")))
                            (anthropic--signal-protocol-failure
                             "The provider returned invalid initial usage."
                             :headers headers :response-id response-id :data data))
                          (setf start-usage usage)))
                      (funcall event-callback
                               (make-instance 'provider-progress-event)))
                     ((string= type "content_block_start")
                      (require-started data)
                      (reject-late-block-event data)
                      (let* ((index
                               (anthropic--event-index
                                event :headers headers :response-id response-id
                                :data data))
                             (block (json-get event "content_block")))
                        (when (gethash index seen-blocks)
                          (anthropic--signal-protocol-failure
                           "The provider returned a duplicate content block."
                           :headers headers :response-id response-id :data data))
                        (when (plusp (hash-table-count open-blocks))
                          (anthropic--signal-protocol-failure
                           "The provider started a content block before the previous block stopped."
                           :headers headers :response-id response-id :data data))
                        (unless (= index (hash-table-count seen-blocks))
                          (anthropic--signal-protocol-failure
                           "The provider returned an out-of-order content block index."
                           :headers headers :response-id response-id :data data))
                        (unless (json-object-p block)
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid content block."
                           :headers headers :response-id response-id :data data))
                        (let ((block-type (json-get block "type")))
                          (unless (json-string-member-p block-type '("text" "tool_use"))
                            (anthropic--signal-protocol-failure
                             (format nil
                                     "The provider returned an unsupported content block: ~A."
                                     (or block-type "unknown"))
                             :headers headers :response-id response-id :data data))
                          (let ((state (anthropic--block-state index block-type)))
                            (cond
                              ((string= block-type "text")
                               (multiple-value-bind (text present-p)
                                   (gethash "text" block)
                                 (when (and present-p (not (stringp text)))
                                   (anthropic--signal-protocol-failure
                                    "The provider returned invalid initial text."
                                    :headers headers :response-id response-id
                                    :data data))
                                 (when (and present-p (plusp (length text)))
                                   (write-string
                                    text
                                    (anthropic--block-state-text-stream state))
                                   (funcall
                                    event-callback
                                    (make-instance 'assistant-delta-event
                                                   :text text)))))
                              ((string= block-type "tool_use")
                               (let ((id (json-get block "id"))
                                     (name (json-get block "name")))
                                 (unless (and (non-empty-string-p id)
                                              (non-empty-string-p name))
                                   (anthropic--signal-protocol-failure
                                    "The provider returned an incomplete tool-use block."
                                    :headers headers :response-id response-id
                                    :data data))
                                 (setf (anthropic--block-state-id state) id
                                       (anthropic--block-state-name state) name))
                               (multiple-value-bind (input present-p)
                                   (gethash "input" block)
                                 (when (and present-p (not (json-object-p input)))
                                   (anthropic--signal-protocol-failure
                                    "The provider returned invalid initial tool input."
                                    :headers headers :response-id response-id
                                    :data data))
                                 (when present-p
                                   (setf (anthropic--block-state-initial-input state)
                                         input)))))
                            (setf (gethash index seen-blocks) t
                                  (gethash index open-blocks) state)))
                      (funcall event-callback
                               (make-instance 'provider-progress-event))))
                     ((string= type "content_block_delta")
                      (require-started data)
                      (reject-late-block-event data)
                      (let* ((index
                               (anthropic--event-index
                                event :headers headers :response-id response-id
                                :data data))
                             (state (gethash index open-blocks))
                             (delta (json-get event "delta")))
                        (unless state
                          (anthropic--signal-protocol-failure
                           "The provider returned a delta for an unopened content block."
                           :headers headers :response-id response-id :data data))
                        (unless (json-object-p delta)
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid content block delta."
                           :headers headers :response-id response-id :data data))
                        (let ((delta-type (json-get delta "type")))
                          (cond
                            ((and (string= (anthropic--block-state-type state) "text")
                                  (json-string= delta-type "text_delta"))
                             (let ((text (json-get delta "text")))
                               (unless (stringp text)
                                 (anthropic--signal-protocol-failure
                                  "The provider returned invalid text delta content."
                                  :headers headers :response-id response-id
                                  :data data))
                               (write-string
                                text (anthropic--block-state-text-stream state))
                               (funcall event-callback
                                        (make-instance 'assistant-delta-event
                                                       :text text))))
                            ((and (string= (anthropic--block-state-type state)
                                          "tool_use")
                                  (json-string= delta-type "input_json_delta"))
                             (let ((partial-json (json-get delta "partial_json")))
                               (unless (stringp partial-json)
                                 (anthropic--signal-protocol-failure
                                  "The provider returned invalid tool input JSON."
                                  :headers headers :response-id response-id
                                  :data data))
                               (write-string
                                partial-json
                                (anthropic--block-state-json-stream state))
                               (funcall event-callback
                                        (make-instance 'provider-progress-event))))
                            (t
                             (anthropic--signal-protocol-failure
                              (format nil
                                      "The provider returned an unsupported ~A delta for a ~A block."
                                      (or delta-type "unknown")
                                      (anthropic--block-state-type state))
                              :headers headers :response-id response-id
                              :data data))))))
                     ((string= type "content_block_stop")
                      (require-started data)
                      (reject-late-block-event data)
                      (let* ((index
                               (anthropic--event-index
                                event :headers headers :response-id response-id
                                :data data))
                             (state (gethash index open-blocks)))
                        (unless state
                          (anthropic--signal-protocol-failure
                           "The provider stopped an unopened content block."
                           :headers headers :response-id response-id :data data))
                        (setf (gethash index completed-blocks)
                              (anthropic--block-item
                               state :headers headers :response-id response-id
                               :data data))
                        (remhash index open-blocks))
                      (funcall event-callback
                               (make-instance 'provider-progress-event)))
                     ((string= type "message_delta")
                      (require-started data)
                      (when (plusp (hash-table-count open-blocks))
                        (anthropic--signal-protocol-failure
                         "The provider returned message_delta with an open content block."
                         :headers headers :response-id response-id :data data))
                      (setf message-delta-seen-p t)
                      (multiple-value-bind (usage present-p)
                          (gethash "usage" event)
                        (unless (and present-p
                                     (anthropic--usage-valid-p
                                      usage '("output_tokens")))
                          (anthropic--signal-protocol-failure
                           "The provider returned invalid delta usage."
                           :headers headers :response-id response-id :data data))
                        (setf delta-usage usage))
                      (let ((delta (json-get event "delta")))
                        (unless (json-object-p delta)
                          (anthropic--signal-protocol-failure
                           "The provider returned an invalid message_delta event."
                           :headers headers :response-id response-id :data data))
                        (multiple-value-bind (reason present-p)
                            (gethash "stop_reason" delta)
                          (when (and present-p reason
                                     (not (non-empty-string-p reason)))
                            (anthropic--signal-protocol-failure
                             "The provider returned an invalid stop reason."
                             :headers headers :response-id response-id :data data))
                          (when (and present-p reason)
                            (when (and stop-reason
                                       (not (string= stop-reason reason)))
                              (anthropic--signal-protocol-failure
                               "The provider changed its stop reason mid-stream."
                               :headers headers :response-id response-id
                               :data data))
                            (setf stop-reason reason
                                  turn-completion
                                  (anthropic--stop-reason-completion
                                   reason :headers headers
                                   :response-id response-id :data data)))))
                      (funcall event-callback
                               (make-instance 'provider-progress-event)))
                     ((string= type "message_stop")
                      (require-started data)
                      (unless message-delta-seen-p
                        (anthropic--signal-protocol-failure
                         "The provider stopped before message_delta."
                         :headers headers :response-id response-id :data data))
                      (when (plusp (hash-table-count open-blocks))
                        (anthropic--signal-protocol-failure
                         "The provider stopped with an open content block."
                         :headers headers :response-id response-id :data data))
                      (unless (non-empty-string-p stop-reason)
                        (anthropic--signal-protocol-failure
                         "The provider stopped without a stop reason."
                         :headers headers :response-id response-id :data data))
                      (let* ((items
                               (anthropic--ordered-block-items
                                completed-blocks :headers headers
                                :response-id response-id :data data))
                             (tool-calls
                               (remove-if-not
                                #'clinker-transcript:function-call-item-p items)))
                        (when (and (string= stop-reason "tool_use")
                                   (null tool-calls))
                          (anthropic--signal-protocol-failure
                           "The provider reported tool_use without a tool call."
                           :headers headers :response-id response-id :data data))
                        (when (and tool-calls
                                   (not (string= stop-reason "tool_use")))
                          (anthropic--signal-protocol-failure
                           "The provider returned a tool call without tool_use."
                           :headers headers :response-id response-id :data data))
                        (setf output-items items
                              completed-p t)
                        (dolist (item output-items)
                          (funcall event-callback
                                   (make-instance 'provider-item-event :item item)))
                        (funcall
                         event-callback
                         (make-instance
                          'provider-completed-event
                          :response-id response-id
                          :usage (anthropic--portable-usage start-usage delta-usage)
                          :turn-completion turn-completion))))
                     (t
                      ;; Anthropic may add top-level event types. Ignore their
                      ;; unknown metadata without discarding content blocks.
                      (funcall event-callback
                               (make-instance 'provider-progress-event))))))))
    (make-instance 'provider-result
                   :response-id response-id
                   :output-items output-items
                   :tool-calls (remove-if-not
                                #'clinker-transcript:function-call-item-p output-items)
                   :usage (anthropic--portable-usage start-usage delta-usage)
                   :turn-state nil
                   :turn-completion turn-completion)))


;;;; -- Projected Anthropic Requests --

(defmethod provider-wire-protocol ((provider anthropic-messages-provider))
  "Identify the Anthropic Messages wire protocol."
  (declare (ignore provider))
  ':anthropic-messages)

(defmethod provider-wire-tool-name
    ((provider anthropic-messages-provider) (namespace string) (name string))
  "Encode a reversible namespaced Anthropic function name."
  (declare (ignore provider))
  (openai-compatible--wire-tool-name namespace name))

(defmethod provider-wire-tool
    ((provider anthropic-messages-provider) (namespace string) (tool hash-table))
  "Encode one projected tool for the Anthropic Messages protocol."
  (declare (ignore provider))
  (anthropic--wire-tool namespace tool))

(defmethod provider-wire-tools
    ((provider anthropic-messages-provider) (tool-namespaces vector))
  "Flatten projected tool namespaces into Anthropic declarations."
  (declare (ignore provider))
  (anthropic--wire-tools tool-namespaces))

(defmethod provider-request-object
    ((provider anthropic-messages-provider) (projection wire-request) (tools vector)
     &key goal-context compaction-p)
  "Encode projected messages, instruction texts, tools, and cache boundaries.

ITEMS is durable history. The :EPHEMERAL-ITEMS option and SUFFIX texts follow
that history without a cache marker. PREFIX contains stable system texts.
:CACHE-P defaults to true; callers disable it for one-off requests. TOOLS is
the result of PROVIDER-WIRE-TOOLS. Caller-owned inputs are never annotated."
  (declare (ignore provider goal-context compaction-p))
  (let* ((options (wire-request-options projection))
         (cache-p (getf options :cache-p t))
         (history (anthropic--input-messages (wire-request-items projection)))
         (messages
           (anthropic--append-user-texts
            (anthropic--append-messages
             (if cache-p
                 (anthropic--mark-history-cache-breakpoint history)
                 history)
             (anthropic--input-messages (getf options :ephemeral-items)))
            (wire-request-suffix projection)))
         (system (anthropic--system-blocks (wire-request-prefix projection)
                                          :cache-p cache-p))
         (wire-tools (map 'vector #'json-object-copy tools))
         (request
           (json-object
            "model" (wire-request-model projection)
            "max_tokens" (min (or (getf options :maximum-output-tokens)
                                  *anthropic-maximum-output-tokens*)
                              *anthropic-maximum-output-tokens*)
            "messages" (coerce messages 'vector)
            "stream" t)))
    (when system
      (setf (gethash "system" request) system))
    (when (plusp (length wire-tools))
      (when cache-p
        (anthropic--mark-tool-cache-breakpoint wire-tools))
      (setf (gethash "tools" request) wire-tools
            (gethash "tool_choice" request) (json-object "type" "auto")))
    request))
