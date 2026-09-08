
(asdf/parse-defsystem:defsystem #:cl-llm-provider-api
  :description
  "Portable protocols and value types for language-model providers."
  :author
  "Lambda Symbolics OÜ"
  :license
  "COLL-Attribution"
  :version
  "0.2.0"
  :serial
  t
  :depends-on
  (#:babel #:bordeaux-threads #:ironclad)
  :components
  ((:module "src" :serial t :components
    ((:file "package") (:file "provider") (:file "engine")
     (:file "inference-budget") (:file "inference-view")
     (:file "inference-object"))))
  :in-order-to
  ((asdf/lisp-action:test-op
    (asdf/lisp-action:test-op #:cl-llm-provider-api/tests))))

(asdf/parse-defsystem:defsystem #:cl-llm-provider-api/tests
  :description
  "Tests for cl-llm-provider-api."
  :depends-on
  (#:cl-llm-provider-api/dexador #:cl-llm-provider-api/context
   #:cl-llm-provider-api/contracts)
  :serial
  t
  :components
  ((:module "tests" :serial t :components
    ((:file "tests") (:file "request") (:file "wire") (:file "context-tests")
     (:file "contract-tests") (:file "chat") (:file "anthropic"))))
  :perform
  (asdf/lisp-action:test-op (operation component)
   (declare (ignore operation component))
   (uiop/package:symbol-call '#:cl-llm-provider-api/tests '#:run-tests)))

(asdf/parse-defsystem:defsystem #:cl-llm-provider-api/wire
  :description
  "Concrete Responses, Chat Completions, and Messages protocols."
  :depends-on
  (#:cl-llm-provider-api #:yason #:clinker-transcript #:cl-rfc8628)
  :serial
  t
  :components
  ((:module "src" :serial t :components
    ((:file "wire-json") (:file "wire-conditions") (:file "wire-transport")
     (:file "wire-items") (:file "wire-client") (:file "responses")
     (:file "chat-policy") (:file "chat-completions") (:file "usage")
     (:file "request") (:file "anthropic")))))

(asdf/parse-defsystem:defsystem #:cl-llm-provider-api/dexador
  :description
  "Optional Dexador condition normalization for provider transports."
  :depends-on
  (#:cl-llm-provider-api/wire #:dexador #:cl+ssl #:usocket)
  :components
  ((:file "src/dexador")))

(asdf/parse-defsystem:defsystem #:cl-llm-provider-api/context
  :description
  "Request-local context assembly."
  :depends-on
  (#:cl-llm-provider-api)
  :components
  ((:file "src/context")))

(asdf/parse-defsystem:defsystem #:cl-llm-provider-api/contracts
  :description
  "Structured output contracts and exact JSON values."
  :depends-on
  (#:cl-llm-provider-api #:yason)
  :components
  ((:file "src/contracts")))
