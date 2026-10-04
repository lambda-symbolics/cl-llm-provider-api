(in-package #:cl-llm-provider-api)

;;;; -- Deferred Namespace and Tool Search Tests --

(defun tool-search-test--namespaces ()
  "Return two deferred namespaces used by the tool search tests."
  (json-array
   (json-object
    "type" "namespace" "name" "resource"
    "description" "Revision-gated observation of resources."
    "tools" (json-array
             (json-object "type" "function" "name" "read"
                          "description" "Read one resource window."
                          "parameters" (json-object "type" "object"))
             (json-object "type" "function" "name" "edit"
                          "description" "Apply structured edits."
                          "parameters" (json-object "type" "object"))))
   (json-object
    "type" "namespace" "name" "shell" "description" "External commands."
    "tools" (json-array
             (json-object "type" "function" "name" "run"
                          "description" "Run one shell command in the workspace."
                          "parameters" (json-object "type" "object"))))))

(defun tool-search-test--shape (tools)
  "Return TOOLS as a list of (namespace tool-name ...) entries."
  (loop for namespace across tools
        collect (cons (json-get namespace "name")
                      (loop for tool across (json-get namespace "tools")
                            collect (json-get tool "name")))))

(defun tool-search-test--scoring ()
  "Test scoring, whole-namespace expansion, limits, and unreadable arguments."
  (let ((namespaces (tool-search-test--namespaces)))
    (dolist (case '(("resource" nil (("resource" "read" "edit")))
                    ("read" nil (("resource" "read")))
                    ("run the command" nil (("shell" "run")))
                    ("edit" nil (("resource" "edit")))
                    ("workspace" nil (("shell" "run")))
                    ("read edit run" 1 (("resource" "read")))
                    ("resource run" 1 (("resource" "read" "edit") ("shell" "run")))
                    ("nothing here" nil ())
                    ("" nil ())))
      (destructuring-bind (query limit expected) case
        (multiple-value-bind (terms effective-limit)
            (provider-tool-search-terms (if limit
                                            (json-object "query" query "limit" limit)
                                            (json-object "query" query)))
          (test-assert (equal (tool-search-test--shape
                               (provider-tool-search namespaces terms :limit effective-limit))
                              expected)
                       (format nil "query ~S limit ~S exposes ~S" query limit expected)))))
    (let* ((output (provider-tool-search-output
                    (json-object "type" "tool_search_call" "call_id" "search-7"
                                 "execution" "client" "arguments" "{\"query\":\"shell\"}")
                    namespaces))
           (child (aref (json-get (aref (json-get output "tools") 0) "tools") 0)))
      (test-assert (and (json-string= (json-get output "type") "tool_search_output")
                        (json-string= (json-get output "call_id") "search-7")
                        (json-string= (json-get output "status") "completed")
                        (json-string= (json-get output "execution") "client")
                        (equal (tool-search-test--shape (json-get output "tools"))
                               '(("shell" "run")))
                        (eq (json-get child "defer_loading") t)
                        (nth-value 1 (json-get-present child "strict")))
                   "a JSON-string query answers with a client tool_search_output"))
    (test-assert (equal (tool-search-test--shape
                         (json-get (provider-tool-search-output
                                    (json-object "type" "tool_search_call" "call_id" "search-8"
                                                 "execution" "client"
                                                 "arguments" (json-object
                                                              "paths" (json-array "shell")))
                                    namespaces)
                                   "tools"))
                        '(("shell" "run")))
                 "the paths argument names namespaces")
    (test-assert (zerop (length (json-get (provider-tool-search-output
                                           (json-object "type" "tool_search_call"
                                                        "call_id" "search-9"
                                                        "execution" "client"
                                                        "arguments" "not json")
                                           namespaces)
                                          "tools")))
                 "unreadable search arguments expose nothing")))

(defun tool-search-test--wire-forms ()
  "Test the deferred declarations, call recognition, and replay shapes."
  (let* ((tools (provider-deferred-wire-tools
                 (concatenate 'vector (tool-search-test--namespaces)
                              (vector (json-object "type" "web_search")))
                 :description "Find tools."))
         (child (aref (json-get (aref tools 0) "tools") 0))
         (search-tool (aref tools 3)))
    (test-assert (and (= (length tools) 4)
                      (eq (json-get child "defer_loading") t)
                      (json-string= (json-get (aref tools 2) "type") "web_search")
                      (json-string= (json-get search-tool "type") "tool_search")
                      (json-string= (json-get search-tool "execution") "client")
                      (json-string= (json-get search-tool "description") "Find tools.")
                      (json-object-p (json-get (json-get (json-get search-tool "parameters")
                                                         "properties")
                                               "query")))
                 "namespaces defer their tools and one client tool_search follows")
    (test-assert (= (length (provider-deferred-wire-tools
                             (vector (json-object "type" "function" "name" "plain"))))
                    1)
                 "no tool_search is declared without a deferred namespace"))
  (test-assert (and (provider-tool-search-call-p
                     (json-object "type" "tool_search_call" "execution" "client"))
                    (not (provider-tool-search-call-p
                          (json-object "type" "tool_search_call" "execution" "server"))))
               "only client-executed tool search calls need an answer")
  (let* ((output (json-object
                  "type" "tool_search_output" "status" "completed" "call_id" "search-1"
                  "execution" "client"
                  "tools" (json-array
                           (json-object
                            "type" "namespace" "name" "plan" "description" "Plans."
                            "tools" (json-array
                                     (json-object "type" "function" "name" "update"
                                                  "description" "Update the plan."
                                                  "defer_loading" t
                                                  "parameters" (json-object "type" "object")
                                                  "output_schema" nil)
                                     (json-object "type" "function" "name" "broken"
                                                  "defer_loading" t "parameters" nil))))))
         (replayed (provider-tool-search-output-replay output))
         (namespace (aref (json-get replayed "tools") 0))
         (child (aref (json-get namespace "tools") 0)))
    (test-assert (and (= (length (json-get namespace "tools")) 1)
                      (json-string= (json-get child "name") "update")
                      (eq (json-get child "defer_loading") t)
                      (not (nth-value 1 (json-get-present child "output_schema")))
                      (= (length (json-get output "tools")) 1))
                 "replay rebuilds expansions in the deferred shape without mutating them")
    (test-assert (zerop (length (json-get (provider-tool-search-output-replay
                                           output :trimming-p t)
                                          "tools")))
                 "trimmed history replays expansions empty")))

(defun run-tool-search-tests ()
  "Run the deferred namespace and tool search checks and return their count."
  (let ((*wire-test-checks* 0))
    (tool-search-test--scoring)
    (tool-search-test--wire-forms)
    *wire-test-checks*))
