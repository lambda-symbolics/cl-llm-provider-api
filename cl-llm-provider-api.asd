(asdf:defsystem #:cl-llm-provider-api
  :description "Portable protocols and value types for language-model providers."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.2.0"
  :serial t
  :depends-on (#:babel
               #:bordeaux-threads
               #:ironclad)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "provider")
                             (:file "engine")
                             (:file "inference-budget")
                             (:file "inference-view")
                             (:file "inference-object"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-llm-provider-api/tests))))

(asdf:defsystem #:cl-llm-provider-api/tests
  :description "Tests for cl-llm-provider-api."
  :depends-on (#:cl-llm-provider-api)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:cl-llm-provider-api/tests '#:run-tests)))
