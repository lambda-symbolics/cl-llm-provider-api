
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
  (#:cl-llm-provider-api/context #:cl-llm-provider-api/contracts)
  :serial
  t
  :components
  ((:module "tests" :serial t :components
    ((:file "tests") (:file "context-tests") (:file "contract-tests"))))
  :perform
  (asdf/lisp-action:test-op (operation component)
   (declare (ignore operation component))
   (uiop/package:symbol-call '#:cl-llm-provider-api/tests '#:run-tests)))

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
