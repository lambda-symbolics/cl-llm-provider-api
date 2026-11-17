(in-package #:cl-llm-provider-api)

;;;; -- Encoded Request Analysis Tests --

(defclass request-analysis-test-responses (responses-api-provider) ()
  (:documentation "Responses test transport with remote output enforcement."))

(defclass request-analysis-test-chat (chat-completions-provider) ()
  (:documentation "Chat test transport with remote output enforcement."))

(defmethod provider-output-ceiling-p ((provider request-analysis-test-responses))
  "Enable the standard ceiling writer for this test transport."
  (declare (ignore provider))
  t)

(defmethod provider-output-ceiling-p ((provider request-analysis-test-chat))
  "Enable the standard ceiling writer for this test transport."
  (declare (ignore provider))
  t)

(defun test-request-analysis ()
  "Exercise declared wire shapes, opaque fallbacks, detached copies and image uncertainty."
  (flet ((check (value description)
           "Count a public library contract assertion."
           (cl-llm-provider-api/tests::check value description)))
    (dolist
        (case
         '((request-analysis-test-responses "input" ("max_output_tokens")
            "input_image")
           (request-analysis-test-chat "messages" ("max_completion_tokens")
            "image_url")
           (anthropic-messages-provider "messages" ("max_tokens") "image")
           (gemini-generate-content-provider "contents"
            ("generationConfig" "maxOutputTokens") nil)))
      (destructuring-bind
          (class field path image-type)
          case
        (let* ((provider (make-instance class))
               (control  (json-object "type" "ephemeral" "ttl" "1h"))
               (item
                (json-object "content"
                             (vector
                              (json-object "type" "text" "text" "hello"
                                           "cache_control" control))))
               (history  (vector item))
               (request
                (json-object "model" "test" field history "request"
                             (json-object "opaque" "schema")))
               (snapshot (json-encode request))
               (candidate
                (provider-request-with-output-ceiling provider request 91)))
          (multiple-value-bind (items name)
              (provider-request-history provider request)
            (check (and (eq items history) (equal name field))
             "history follows the declared encoder field"))
          (check (eq request (provider-request-body provider request))
           "ordinary encoders retain arbitrary request fields")
          (check (equal path (first (provider-request-output-paths provider)))
           "output paths follow the encoder")
          (check (= 91 (request-analysis--path-value candidate path))
           "the ceiling writer uses the declared path")
          (check
           (equal '(91) (provider-request-output-limits provider candidate))
           "encoded ceiling readback agrees")
          (check (string= snapshot (json-encode request))
           "ceiling and analysis preserve caller-owned request objects")
          (check
           (equal
            (when
                (member class
                        '(request-analysis-test-chat
                          anthropic-messages-provider))
              (list (list (list field 0 "content" 0) "1h")))
            (provider-request-cache-breakpoints provider request))
           "cache markers retain root-first paths and lifetimes")
          (check
           (string=
            (json-encode (provider-request-context-body provider request))
            (json-encode (provider-request-context-body provider candidate)))
           "output-only changes do not invalidate measured-input comparison")
          (let ((image
                 (if image-type
                     (json-object "type" image-type)
                     (json-object "inlineData"
                                  (json-object "mimeType" "image/png" "data"
                                               "encoded")))))
            (multiple-value-bind (characters uncertain-p)
                (provider-request-characters provider image :image-characters
                                             4096)
              (check (and (= characters 4096) uncertain-p)
               "host image allowance applies to each encoder's image shape"))))))
    (let* ((provider (make-instance 'model-provider))
           (request
            (json-object "request"
                         (json-object "input"
                                      (vector
                                       (make-string 300 :initial-element #\x)))
                         "input" #("outer") "messages" #("other") "max_tokens"
                         73 "cache_control" (json-object "type" "ephemeral"))))
      (check (eq request (provider-request-body provider request))
       "opaque envelopes are not guessed")
      (check
       (equal '(nil nil)
              (multiple-value-list
               (provider-request-history provider request)))
       "unknown transports have no inferred history")
      (check (equal '(73) (provider-request-output-limits provider request))
       "unknown positive limits are read conservatively")
      (check
       (eq request (provider-request-with-output-ceiling provider request 12))
       "unknown transports do not claim remote enforcement")
      (check (eq request (provider-request-context-body provider request))
       "opaque comparison retains every field")
      (check (null (provider-request-cache-breakpoints provider request))
       "opaque marker semantics are not inferred")
      (check (> (provider-request-characters provider request) 300)
       "opaque nested input contributes to cold estimation"))
    (let* ((provider (make-instance 'gemini-generate-content-provider))
           (settings (json-object "temperature" 0.4 "maxOutputTokens" 300))
           (request  (json-object "generationConfig" settings "max_tokens" 100))
           (candidate
            (provider-request-with-output-ceiling provider request 75)))
      (check
       (and (not (eq settings (json-get candidate "generationConfig")))
            (= 300 (json-get settings "maxOutputTokens"))
            (= 0.4
               (json-get (json-get candidate "generationConfig")
                         "temperature")))
       "nested ceiling writes detach settings and preserve unrelated fields")
      (check
       (equal '(100 300)
              (sort (provider-request-output-limits provider request) #'<))
       "common positive caller limits remain available to host min/max policy"))
    (let ((provider (make-instance 'model-provider)))
      (dolist
          (case
           (list (list "abc" 5 nil) (list #("a" "bc") 11 nil)
                 (list (json-object "hello" "x") 14 nil)
                 (list (json-object "encrypted_content" "x") 26 t)))
        (destructuring-bind
            (value count uncertain-p)
            case
          (check
           (equal (list count uncertain-p)
                  (multiple-value-list
                   (provider-request-characters provider value)))
           "character weights and encrypted uncertainty are independent of host token policy")))))
  (test-request-analysis-payload-boundaries)
  t)

(defun test-request-analysis-payload-boundaries ()
  "Preserve schemas and tool JSON while discounting actual wire annotations and images."
  (flet ((check (value description)
           "Count a public request-analysis assertion."
           (cl-llm-provider-api/tests::check value description)))
    (dolist (class '(anthropic-messages-provider request-analysis-test-chat))
      (let* ((provider       (make-instance class))
             (schema
              (json-object "type" "object" "properties"
                           (json-object "cache_control"
                                        (json-object "type" "string"))))
             (payload
              (json-object "cache_control"
                           (json-object "type" "ephemeral" "ttl" "payload")))
             (tool
              (json-object "name" "tool" "input_schema" schema "cache_control"
                           (json-object "type" "ephemeral" "ttl" "1h")))
             (request
              (json-object "tools" (vector tool) "messages"
                           (vector
                            (json-object "role" "assistant" "content"
                                         (vector
                                          (json-object "type" "tool_use"
                                                       "input" payload)
                                          (json-object "type" "text" "text"
                                                       "hello" "cache_control"
                                                       (json-object "type"
                                                                    "ephemeral")))))))
             (snapshot       (json-encode request))
             (canonical      (provider-request-context-body provider request))
             (canonical-tool (aref (json-get canonical "tools") 0))
             (canonical-content
              (json-get (aref (json-get canonical "messages") 0) "content")))
        (check
         (equal '((("tools" 0) "1h") (("messages" 0 "content" 1) nil))
                (provider-request-cache-breakpoints provider request))
         "cache markers are limited to declared annotation objects")
        (check
         (and
          (json-get
           (json-get (json-get canonical-tool "input_schema") "properties")
           "cache_control")
          (json-get (json-get (aref canonical-content 0) "input")
                    "cache_control"))
         "schema properties and tool-input cache_control data survive canonicalization")
        (check
         (and (null (json-get canonical-tool "cache_control"))
              (null (json-get (aref canonical-content 1) "cache_control")))
         "actual wire annotations are removed from measured-input comparison")
        (check (string= snapshot (json-encode request))
         "canonicalization does not mutate source schemas or payloads")
        (setf (gethash "type"
                       (json-get (json-get schema "properties")
                                 "cache_control"))
              "integer")
        (check
         (not
          (string= (json-encode canonical)
                   (json-encode
                    (provider-request-context-body provider request))))
         "a schema property change invalidates the measured-input comparison")))
    (let* ((provider      (make-instance 'gemini-generate-content-provider))
           (image
            (json-object "inlineData"
                         (json-object "mimeType" "image/png" "data"
                                      (make-string 300000 :initial-element
                                                   #\x))))
           (image-message (json-object "role" "user" "parts" (vector image))))
      (multiple-value-bind (characters uncertain-p)
          (provider-request-characters provider image :image-characters 65536)
        (check (and (= characters 65536) uncertain-p)
         "a directly supplied Gemini image part uses the host allowance"))
      (dolist
          (request
           (list (json-object "contents" (vector image-message))
                 (vector image-message)))
        (multiple-value-bind (characters uncertain-p)
            (provider-request-characters provider request :image-characters
                                         65536)
          (check (and uncertain-p (<= 65536 characters 66000))
           "encoded Gemini history and retained-history additions recognize actual image parts")))
      (dolist
          (payload
           (list image
                 (json-object "mimeType" "image/png" "text"
                              (make-string 300000 :initial-element #\x))))
        (dolist
            (request
             (list
              (json-object "contents"
                           (vector
                            (json-object "role" "model" "parts"
                                         (vector
                                          (json-object "functionCall"
                                                       (json-object "name"
                                                                    "tool"
                                                                    "args"
                                                                    payload))))))
              (json-object "contents"
                           (vector
                            (json-object "role" "user" "parts"
                                         (vector
                                          (json-object "functionResponse"
                                                       (json-object "name"
                                                                    "tool"
                                                                    "response"
                                                                    payload))))))
              (json-object "tools"
                           (vector
                            (json-object "functionDeclarations"
                                         (vector
                                          (json-object "name" "tool"
                                                       "parameters"
                                                       payload)))))))
          (check
           (>
            (provider-request-characters provider request :image-characters
                                         65536)
            300000)
           "image-shaped tool arguments, results and schemas count their complete JSON"))))
    (dolist
        (class
         '(request-analysis-test-responses gemini-generate-content-provider))
      (let* ((provider (make-instance class))
             (request
              (json-object "tools"
                           (vector
                            (json-object "parameters"
                                         (json-object "cache_control"
                                                      (json-object "type"
                                                                   "ephemeral")))))))
        (check (null (provider-request-cache-breakpoints provider request))
         "protocols without explicit cache_control annotations report no guessed markers")
        (check
         (json-get
          (json-get
           (aref
            (json-get (provider-request-context-body provider request) "tools")
            0)
           "parameters")
          "cache_control")
         "opaque tool JSON is retained by protocols without cache_control annotations"))))
  (let ((provider (make-instance 'model-provider))
        (payload
         (json-object "mimeType" "image/png" "text"
                      (make-string 300000 :initial-element #\x))))
    (cl-llm-provider-api/tests::check
     (> (provider-request-characters provider payload :image-characters 65536)
        300000)
     "an opaque root image lookalike counts its complete JSON"))
  t)
