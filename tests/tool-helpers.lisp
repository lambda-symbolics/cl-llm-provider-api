(in-package #:cl-llm-provider-api/tests)


(defun test-tool-helpers ()
  "Exercise provider identifier, schema, and namespace helper contracts."
  (let* ((mapping (provider-tool-identifier-map
                   '("hello world" "9 lives" "žluťoučký")
                   :identity-scope "server"))
         (identifier (gethash "hello world" mapping)))
    (check (<= (length identifier) *provider-chat-completions-tool-identifier-limit*)
           "identifier exceeded the provider character budget")
    (check (string= (subseq identifier 0 12) "hello_world_")
           "identifier no longer has the stable readable prefix: ~A" identifier)
    (check (every (lambda (character)
                    (or (alphanumericp character)
                        (member character '(#\_ #\-) :test #'char=)))
                  identifier)
           "identifier contains a non-provider-safe character")
    (check (equalp mapping
                   (provider-tool-identifier-map
                    '("žluťoučký" "9 lives" "hello world")
                    :identity-scope "server"))
           "identifier mapping depends on discovery order")
    (check (string= identifier "hello_world_53F16F219F")
           "scoped UTF-8 FNV identifier does not match its stable identity")
    (check (not (equal (gethash "hello world" mapping)
                       (gethash "hello world"
                                (provider-tool-identifier-map
                                 '("hello world") :identity-scope "other"))))
           "identity scope did not affect the hash")
    (check (handler-case
               (progn (provider-tool-identifier-map '("same" "same")) nil)
             (provider-tool-identifier-duplicate-error () t))
           "duplicate raw names were not typed")
    (check (handler-case
               (progn (provider-tool-identifier-map '("x") :limit 11) nil)
             (provider-tool-identifier-limit-error () t))
           "minimum limit was not enforced")
    (check (handler-case
               (progn (provider-tool-identifier-map '("x") :limit 24) nil)
             (provider-tool-identifier-limit-error () t))
           "maximum limit was not enforced")
    (check (handler-case
               (progn (provider-tool-identifier-map '("x") :prefix "bad.prefix") nil)
             (provider-tool-identifier-error () t))
           "unsafe prefix was silently accepted")
    (check (search "unnamed_" (gethash "" (provider-tool-identifier-map '(""))))
           "empty raw names did not receive a safe base")
    (dolist (prefix '("é" "ascii_é"))
      (check (handler-case
                 (progn (provider-tool-identifier-map '("x") :prefix prefix) nil)
               (provider-tool-identifier-error () t))
             "a non-ASCII prefix was accepted"))
    (let* ((namespace
             (gethash "x000001"
                      (provider-tool-identifier-map '("x000001") :prefix "mcp__")))
           (name
             (gethash "_x000001_"
                      (provider-tool-identifier-map '("_x000001_")
                                                    :identity-scope "x000001")))
           (wire (provider-wire-function-name--encode namespace name)))
      (check (<= (length wire) *provider-wire-function-name-maximum-length*)
             "reserved escape spelling exceeded the combined wire-name budget")
      (check (equal (multiple-value-list (provider-wire-function-name--decode wire))
                    (list namespace name))
             "reserved escape spelling changed a decoded identity"))
    (let ((original (symbol-function 'cl-llm-provider-api::provider-tool-identifier--hash)))
      (unwind-protect
           (progn
             (setf (symbol-function 'cl-llm-provider-api::provider-tool-identifier--hash)
                   (lambda (raw)
                     (declare (ignore raw))
                     "0000000000000000"))
             (check (handler-case
                        (progn
                          (provider-tool-identifier-map '("hello world" "hello/world"))
                          nil)
                      (provider-tool-identifier-collision-error () t))
                    "a hash collision did not produce a typed identifier failure"))
        (setf (symbol-function 'cl-llm-provider-api::provider-tool-identifier--hash)
              original))))
  (let* ((properties (cl-llm-provider-api::json-object "a" (provider-string-property "A")
                                  "flag" (provider-boolean-property "Flag")))
         (closed (provider-object-schema properties '("a")))
         (open (provider-open-object-schema properties :required '("a")))
         (false-args (cl-llm-provider-api::json-object "a" cl-llm-provider-api::*json-decoded-false*)))
    (check (eq (gethash "additionalProperties" closed) cl-llm-provider-api::*json-decoded-false*)
           "closed schema was not closed")
    (check (null (gethash "additionalProperties" open))
           "open schema unexpectedly declared additional properties")
    (check (null (provider-schema-missing-required-names closed false-args))
           "false-valued required property was treated as absent")
    (check (equal (provider-schema-missing-required-names closed (cl-llm-provider-api::json-object)) '("a"))
           "missing required property was not reported")
    (check (null (provider-schema-required-names
                  (cl-llm-provider-api::json-object "required" "not-an-array")))
           "string required value was accepted as an array")
    (check (equal (provider-schema-argument-placeholder
                   (cl-llm-provider-api::json-object "type" "string")) "")
           "string placeholder was not conservative")
    (check (eq (provider-schema-argument-placeholder
                (cl-llm-provider-api::json-object "type" "boolean")) cl-llm-provider-api::*json-decoded-false*)
           "boolean placeholder was not false")
    (check (equal (provider-schema-argument-placeholder
                   (cl-llm-provider-api::json-object "type" "string" "enum" #())) "")
           "an empty enum suppressed the type-based retry placeholder")
  (dolist (operator '("oneOf" "anyOf"))
    (let ((schema (cl-llm-provider-api::json-object
                   "required" #("base")
                   operator (vector
                             (cl-llm-provider-api::json-object "required" #("left"))
                             (cl-llm-provider-api::json-object "required" #("right"))))))
      (check (equal (provider-schema-required-groups schema)
                    '(("base" "left") ("base" "right")))
             "~A groups were not combined with base requirements" operator)
      (check (equal (provider-schema-missing-required-names
                     schema (cl-llm-provider-api::json-object "base" nil))
                    '(:one-of "left" "right"))
             "~A alternatives did not produce a minimal diagnostic" operator)))
  (let* ((calls 0)
        (groups (provider-group-tool-schemas
                 '(:a :b :c)
                 :namespace-function (lambda (tool) (if (eq tool :c) "two" "one"))
                 :name-function (lambda (tool) (string-downcase (symbol-name tool)))
                 :schema-function (lambda (tool) (incf calls) tool)
                 :description-function (lambda (namespace)
                                         (format nil "description-~A" namespace)))))
    (check (= calls 3) "schema callback did not run once per tool")
    (check (equalp (map 'list (lambda (group) (gethash "name" group)) groups)
                   '("one" "two"))
           "namespace order was not first-encounter order")
    (check (equalp (coerce (gethash "tools" (aref groups 0)) 'list) '(:a :b))
           "tool order was not first-encounter order")
    (check (string= (gethash "description" (aref groups 0)) "description-one")
           "namespace description was not applied once")
    (check (zerop (length (provider-group-tool-schemas
                           '(:a) :canonical-names nil
                           :namespace-function (lambda (tool) (declare (ignore tool)) "one")
                           :name-function (lambda (tool) (declare (ignore tool)) "a")
                           :schema-function #'identity)))
           "explicit NIL canonical filter was not empty"))
  (let ((array (provider-array-schema (provider-string-property "item")))
        (enum (provider-enum-schema '("a" "b") nil))
        (described (provider-enum-schema #("a") "choose one")))
    (check (equal (gethash "type" array) "array") "array schema type was wrong")
    (check (equalp (gethash "enum" enum) #( "a" "b")) "enum values were not vectors")
    (check (null (gethash "description" enum)) "NIL description changed schema shape")
    (check (string= (gethash "description" described) "choose one")
           "enum description was lost")))

)
