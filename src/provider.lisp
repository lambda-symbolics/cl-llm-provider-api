(in-package #:cl-llm-provider-api)

;;;; -- Provider Values --

(deftype turn-completion ()
  "A provider's explicit continuation, completion, or unspecified turn state."
  '(member :continue :end :unspecified))

(defclass provider-event ()
  ()
  (:documentation "A semantic event emitted while consuming a provider stream."))

(defclass provider-progress-event (provider-event)
  ()
  (:documentation
   "Provider activity with no assistant text or completed item to present."))

(defclass assistant-delta-event (provider-event)
  ((text
    :initarg :text
    :reader assistant-delta-event-text
    :type string
    :documentation "The newly received assistant text."))
  (:documentation "An incremental assistant text update."))

(defclass reasoning-delta-event (provider-event)
  ((text
    :initarg :text
    :reader reasoning-delta-event-text
    :type string
    :documentation "The newly received visible reasoning summary text."))
  (:documentation "An incremental visible reasoning summary update."))

(defclass provider-item-event (provider-event)
  ((item
    :initarg :item
    :reader provider-item-event-item
    :type t
    :documentation "The authoritative completed provider output item."))
  (:documentation "A completed provider output item ready for persistence."))

(defclass provider-completed-event (provider-event)
  ((response-id
    :initarg :response-id
    :initform nil
    :reader provider-completed-event-response-id
    :type (or null string)
    :documentation "The provider response identifier, if supplied.")
   (usage
    :initarg :usage
    :initform nil
    :reader provider-completed-event-usage
    :type t
    :documentation "Portable provider usage metadata, if supplied.")
   (turn-completion
    :initarg :turn-completion
    :initform :unspecified
    :reader provider-completed-event-turn-completion
    :type turn-completion
    :documentation "Whether the provider explicitly ended or continued the turn."))
  (:documentation "The successful terminal event for one provider request."))

(defclass provider-retry-event (provider-event)
  ((attempt
    :initarg :attempt
    :reader provider-retry-event-attempt
    :type (integer 1)
    :documentation "The one-based reconnect attempt about to begin.")
   (maximum-attempts
    :initarg :maximum-attempts
    :reader provider-retry-event-maximum-attempts
    :type (integer 1)
    :documentation "The maximum number of reconnect attempts allowed.")
   (delay
    :initarg :delay
    :reader provider-retry-event-delay
    :type real
    :documentation "Seconds to wait before reconnecting."))
  (:documentation "A transient provider stream is about to be retried."))

(defclass provider-result ()
  ((response-id
    :initarg :response-id
    :initform nil
    :reader provider-result-response-id
    :type (or null string)
    :documentation "The provider response identifier, if supplied.")
   (output-items
    :initarg :output-items
    :initform nil
    :reader provider-result-output-items
    :type list
    :documentation "Authoritative completed response items in wire order.")
   (tool-calls
    :initarg :tool-calls
    :initform nil
    :reader provider-result-tool-calls
    :type list
    :documentation "The function-call subset of OUTPUT-ITEMS.")
   (usage
    :initarg :usage
    :initform nil
    :reader provider-result-usage
    :type t
    :documentation "Portable provider usage metadata, if supplied.")
   (turn-state
    :initarg :turn-state
    :initform nil
    :reader provider-result-turn-state
    :type (or null string)
    :documentation "The routing token to replay within the current user turn.")
   (turn-completion
    :initarg :turn-completion
    :initform :unspecified
    :reader provider-result-turn-completion
    :type turn-completion
    :documentation "Whether the provider explicitly ended or continued the turn."))
  (:documentation "The complete semantic result of one provider request."))


;;;; -- Provider Protocol --

(defclass model-provider ()
  ((registration
    :initarg :registration
    :initform nil
    :accessor model-provider-registration
    :type t
    :documentation "Opaque registry metadata associated with this provider."))
  (:documentation "The abstract interface between an agent and a model service."))

(defclass subscription-provider (model-provider)
  ((configuration
    :initarg :configuration
    :initform nil
    :reader provider-configuration
    :type t
    :documentation "Opaque immutable configuration for this provider instance.")
   (credential-manager
    :initarg :credential-manager
    :initform nil
    :reader provider-credential-manager
    :type t
    :documentation "Opaque credential manager used by this provider instance.")
   (session-id
    :initarg :session-id
    :initform nil
    :reader provider-session-id
    :type t
    :documentation "Opaque session identity associated with this provider instance."))
  (:documentation "A model provider backed by subscription or managed credentials."))

(defgeneric provider-family (provider)
  (:documentation "Return the model family keyword PROVIDER serves."))

(defmethod provider-family ((provider model-provider))
  "Identify a provider without a declared family as custom."
  (declare (ignore provider))
  :custom)

(defgeneric provider-with-configuration (provider configuration)
  (:documentation
   "Return PROVIDER reconfigured for CONFIGURATION while preserving session state."))

(defgeneric provider-stream-turn
    (provider conversation
     &key tool-namespaces event-callback goal-context compaction-p)
  (:documentation
   "Stream one model response for CONVERSATION and return a PROVIDER-RESULT."))

(defgeneric provider-consume-stream (provider stream headers event-callback)
  (:documentation
   "Consume a provider wire STREAM and emit semantic events through EVENT-CALLBACK."))

(defgeneric provider-native-compact-conversation
    (provider conversation &key tool-namespaces event-callback)
  (:documentation
   "Return PROVIDER's opaque native compaction item, or NIL when unsupported."))

(defmethod provider-native-compact-conversation
    ((provider model-provider) conversation &key tool-namespaces event-callback)
  "Leave native compaction unavailable unless a provider implements it."
  (declare (ignore provider conversation tool-namespaces event-callback))
  nil)

(defgeneric provider-set-reasoning-summaries (provider enabled-p)
  (:documentation
   "Set whether PROVIDER requests visible reasoning summaries when supported."))

(defmethod provider-set-reasoning-summaries
    ((provider model-provider) enabled-p)
  "Leave providers without reasoning-summary support unchanged."
  (declare (ignore enabled-p))
  provider)

(defgeneric provider-output-ceiling-p (provider)
  (:documentation
   "Return true when PROVIDER's wire protocol accepts an output ceiling field."))

(defmethod provider-output-ceiling-p ((provider model-provider))
  "Refuse output ceilings unless a concrete provider opts in."
  (declare (ignore provider))
  nil)


;;;; -- Provider Wire Protocol --

(defclass responses-api-provider (subscription-provider)
  ()
  (:documentation "A provider using the streaming Responses API wire protocol."))

(defclass chat-completions-provider (subscription-provider)
  ()
  (:documentation
   "A provider using the streaming Chat Completions wire protocol."))

(defgeneric provider-wire-protocol (provider)
  (:documentation "Return the wire protocol family implemented by PROVIDER."))

(defmethod provider-wire-protocol ((provider model-provider))
  "Identify a provider without a declared shared wire protocol."
  (declare (ignore provider))
  :custom)

(defmethod provider-wire-protocol ((provider responses-api-provider))
  "Identify a standard streaming Responses API provider."
  (declare (ignore provider))
  :responses-api)

(defmethod provider-wire-protocol ((provider chat-completions-provider))
  "Identify a streaming Chat Completions provider."
  (declare (ignore provider))
  :chat-completions)

(defgeneric provider-wire-tool-name (provider namespace name)
  (:documentation
   "Return NAME inside NAMESPACE encoded for PROVIDER's function namespace."))

(defmethod provider-wire-tool-name
    ((provider responses-api-provider) (namespace string) (name string))
  "Join a Responses API tool namespace and name with a dot."
  (declare (ignore provider))
  (format nil "~A.~A" namespace name))

(defgeneric provider-wire-tool (provider namespace tool)
  (:documentation "Return namespaced TOOL encoded for PROVIDER's wire protocol."))

(defgeneric provider-wire-tools (provider tool-namespaces)
  (:documentation
   "Return TOOL-NAMESPACES encoded for PROVIDER's request protocol."))

(defgeneric provider-wire-input-item (provider item)
  (:documentation "Return conversation ITEM encoded for PROVIDER's wire protocol."))

(defgeneric provider-responses-wire-effort (provider configuration)
  (:documentation
   "Return CONFIGURATION's reasoning effort encoded for PROVIDER, or NIL."))

(defgeneric provider-responses-reasoning-summary (provider configuration)
  (:documentation
   "Return CONFIGURATION's reasoning summary style for PROVIDER, or NIL."))

(defmethod provider-responses-reasoning-summary
    ((provider responses-api-provider) configuration)
  "Request no reasoning summary style unless a concrete provider opts in."
  (declare (ignore provider configuration))
  nil)

(defgeneric provider-responses-hosted-tools (provider configuration)
  (:documentation
   "Return PROVIDER's server-executed hosted tool declarations for CONFIGURATION."))

(defmethod provider-responses-hosted-tools
    ((provider responses-api-provider) configuration)
  "Expose no hosted tools unless a concrete provider opts in."
  (declare (ignore provider configuration))
  nil)

(defgeneric provider-responses-request-namespaces (provider tool-namespaces)
  (:documentation
   "Return TOOL-NAMESPACES filtered to the local tools PROVIDER can serve."))

(defmethod provider-responses-request-namespaces
    ((provider responses-api-provider) tool-namespaces)
  "Serve every local tool namespace unless a concrete provider excludes one."
  (declare (ignore provider))
  tool-namespaces)

(defgeneric provider-responses-request-fields
    (provider conversation &key compaction-p)
  (:documentation
   "Return PROVIDER-specific alternating fields for one Responses request."))

(defmethod provider-responses-request-fields
    ((provider responses-api-provider) conversation &key compaction-p)
  "Add no provider-specific request fields by default."
  (declare (ignore provider conversation compaction-p))
  nil)

(defgeneric provider-responses-instructions-placement (provider)
  (:documentation
   "Return where PROVIDER places Responses request instructions.

:INPUT uses conversation input items. :TOP-LEVEL uses a top-level instructions
request field."))

(defmethod provider-responses-instructions-placement
    ((provider responses-api-provider))
  "Place instructions in conversation input items by default."
  (declare (ignore provider))
  :input)

(defgeneric provider-request-object
    (provider conversation tool-namespaces &key goal-context compaction-p)
  (:documentation
   "Build PROVIDER's complete request for CONVERSATION.

Implementations may return a second delivery value whose lifecycle is owned by
the caller."))

(defgeneric provider-normalize-output-item (provider item)
  (:documentation
   "Return completed ITEM normalized for persistence, replay, and dispatch."))

(defmethod provider-normalize-output-item ((provider model-provider) item)
  "Keep output items unchanged unless a provider defines normalization."
  (declare (ignore provider))
  item)
