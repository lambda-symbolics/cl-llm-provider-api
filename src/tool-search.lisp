(in-package #:cl-llm-provider-api)

;;;; -- Responses Deferred Namespaces and Tool Search --

;;; A Responses request may declare its namespaces with every function marked
;;; defer_loading, plus one client-executed tool_search tool. The model then
;;; searches for the tools it needs, the client answers with a
;;; tool_search_output naming them, and that output replays in later requests
;;; so the server keeps the expanded tools loaded. The shapes follow the Codex
;;; reference at commit 18194bfd3534ca567d886eac454028dafaa68b6c.

(defparameter *provider-tool-search-default-limit* 8
  "How many deferred tools one tool search exposes when the model sets no limit.")

(defparameter *provider-tool-search-description*
  "Searches the deferred tool namespaces by namespace name, tool name, and description, and exposes the matching tools for the next model call."
  "The default model-visible description of the tool_search tool.")

(defun provider-deferred-namespace-tool (tool)
  "Convert one TOOL schema to a deferred native namespace child."
  (json-object "type" "function"
               "name" (json-get tool "name")
               "description" (json-get tool "description")
               "strict" yason:false
               "defer_loading" t
               "parameters" (json-get tool "parameters")))

(defun provider-deferred-namespace (namespace)
  "Convert one NAMESPACE to the native deferred Responses wire form."
  (json-object "type" "namespace"
               "name" (json-get namespace "name")
               "description" (json-get namespace "description")
               "tools" (map 'vector #'provider-deferred-namespace-tool
                            (json-get namespace "tools"))))

(defun provider-tool-search-tool (&key (description *provider-tool-search-description*))
  "Return the client-executed tool_search declaration for deferred namespaces.

The model asks for a query and an optional limit, the client answers from its
own tools, and the resulting tool_search_output replays in history so the
expansion stays loaded."
  (json-object
   "type" "tool_search"
   "execution" "client"
   "description" description
   "parameters"
   (json-object
    "type" "object"
    "properties"
    (json-object
     "query" (json-object
              "type" "string"
              "description" "Search terms: namespace names, tool names, or words from tool descriptions.")
     "limit" (json-object
              "type" "number"
              "description" (format nil "Maximum number of tools to return. Defaults to ~D."
                                    *provider-tool-search-default-limit*)))
    "required" (json-array "query")
    "additionalProperties" yason:false)))

(defun provider-deferred-wire-tools
    (tool-namespaces &key (description *provider-tool-search-description*))
  "Return TOOL-NAMESPACES with every namespace deferred and a tool_search tool.

Entries that are not namespaces pass through unchanged; the tool_search tool,
described by DESCRIPTION, is added only when at least one namespace was
deferred."
  (let ((deferred-p nil))
    (concatenate
     'vector
     (map 'vector
          (lambda (entry)
            (if (and (json-object-p entry)
                     (json-string= (json-get entry "type") "namespace"))
                (progn
                  (setf deferred-p t)
                  (provider-deferred-namespace entry))
                entry))
          tool-namespaces)
     (if deferred-p
         (vector (provider-tool-search-tool :description description))
         #()))))

(defun provider-tool-search-call-p (item)
  "Return true when ITEM is a tool_search_call the client must answer."
  (and (json-object-p item)
       (json-string= (json-get item "type") "tool_search_call")
       (json-string= (json-get item "execution") "client")
       t))

(defun provider-result-tool-search-calls (result)
  "Return RESULT's client-executed tool search calls in wire order."
  (remove-if-not #'provider-tool-search-call-p (provider-result-output-items result)))

(defun provider-tool-search-terms (arguments)
  "Return the distinct search terms and the tool limit named by tool search ARGUMENTS.

ARGUMENTS is a JSON object or the JSON string of one. The query string
supplies the terms; a paths array, the shape the server-executed search used,
is accepted as namespace names. The second value is the requested positive
limit or *PROVIDER-TOOL-SEARCH-DEFAULT-LIMIT*. Unreadable ARGUMENTS return NIL
and NIL."
  (let ((object (tool-search--arguments arguments)))
    (if object
        (let ((paths (json-get object "paths"))
              (limit (json-get object "limit")))
          (values
           (remove-duplicates
            (append (tool-search--tokens (json-get object "query"))
                    (and (vectorp paths)
                         (not (stringp paths))
                         (loop for path across paths
                               append (tool-search--tokens path))))
            :test #'string=)
           (if (and (realp limit) (>= limit 1))
               (floor limit)
               *provider-tool-search-default-limit*)))
        (values nil nil))))

(defun provider-tool-search
    (tool-namespaces terms &key (limit *provider-tool-search-default-limit*))
  "Return the deferred namespaces of TOOL-NAMESPACES whose tools match TERMS.

At most LIMIT scored tools are exposed, best matches first, except that a term
naming a namespace exposes that whole namespace. An exact tool name outranks a
name fragment, which outranks the namespace name, which outranks a description
word. The result keeps namespace and tool order from TOOL-NAMESPACES and uses
the deferred wire shape, so it is valid both as a fresh tool_search_output and
on every later replay."
  (let ((scored '()))
    (loop for namespace across tool-namespaces
          when (and (json-object-p namespace)
                    (json-string= (json-get namespace "type") "namespace"))
            do (let* ((namespace-name (or (json-get namespace "name") ""))
                      (named-p (and (member (string-downcase namespace-name) terms
                                            :test #'string=)
                                    t))
                      (tools (json-get namespace "tools")))
                 (when (vectorp tools)
                   (loop for tool across tools
                         for position from 0
                         when (json-object-p tool)
                           do (let ((score (tool-search--score namespace-name tool terms)))
                                (when (plusp score)
                                  (push (list :namespace namespace :tool tool :score score
                                              :position position :named-p named-p)
                                        scored)))))))
    (let* ((ordered (stable-sort (nreverse scored) #'>
                                 :key (lambda (entry) (getf entry :score))))
           (exposed (loop for entry in ordered
                          for index from 0
                          when (or (getf entry :named-p) (< index limit))
                            collect entry)))
      (coerce
       (loop for namespace across tool-namespaces
             for children = (remove-if-not (lambda (entry)
                                             (eq (getf entry :namespace) namespace))
                                           exposed)
             when children
               collect (json-object
                        "type" "namespace"
                        "name" (json-get namespace "name")
                        "description" (json-get namespace "description")
                        "tools" (map 'vector
                                     (lambda (entry)
                                       (provider-deferred-namespace-tool (getf entry :tool)))
                                     (sort (copy-list children) #'<
                                           :key (lambda (entry) (getf entry :position))))))
       'vector))))

(defun provider-tool-search-output (call tool-namespaces)
  "Return the tool_search_output answering tool search CALL from TOOL-NAMESPACES.

The output replays in history under CALL's call_id and marks itself
client-executed. Unreadable arguments expose no tools rather than failing."
  (multiple-value-bind (terms limit) (provider-tool-search-terms (json-get call "arguments"))
    (json-object "type" "tool_search_output"
                 "call_id" (json-get call "call_id")
                 "status" "completed"
                 "execution" "client"
                 "tools" (if limit
                             (provider-tool-search tool-namespaces terms :limit limit)
                             (json-array)))))

(defun provider-tool-search-output-replay (item &key trimming-p)
  "Return the tool_search_output ITEM as the next request should replay it.

When TRIMMING-P, as for a compaction request, the expansion replays empty.
Otherwise every namespace is rebuilt in the deferred wire shape so the server
keeps those tools loaded. Children keep only the fields a deferred function
declares: older server-produced expansions carried a null output_schema that
the request validator rejects, and a child without an object parameter schema
cannot be declared, so it is dropped."
  (let ((copy (json-object-copy item))
        (tools (json-get item "tools")))
    (setf (gethash "tools" copy)
          (if (or trimming-p (not (vectorp tools)) (stringp tools))
              (json-array)
              (coerce (loop for entry across tools
                            for replay = (tool-search--replay-namespace entry)
                            when replay
                              collect replay)
                      'vector)))
    copy))

(defun tool-search--arguments (arguments)
  "Return tool search ARGUMENTS as a JSON object, decoding a JSON string, or NIL."
  (cond
    ((json-object-p arguments)
     arguments)
    ((stringp arguments)
     (let ((decoded (handler-case (json-decode arguments)
                      (error ()
                        nil))))
       (and (json-object-p decoded) decoded)))
    (t
     nil)))

(defun tool-search--tokens (text)
  "Return the lowercase alphanumeric words of TEXT at least two characters long."
  (if (stringp text)
      (let ((tokens '())
            (start nil))
        (loop for index from 0 to (length text)
              for boundary-p = (or (= index (length text))
                                   (not (alphanumericp (char text index))))
              do (cond
                   ((and boundary-p start)
                    (when (>= (- index start) 2)
                      (push (string-downcase (subseq text start index)) tokens))
                    (setf start nil))
                   ((and (not boundary-p) (null start))
                    (setf start index))))
        (nreverse tokens))
      '()))

(defun tool-search--token-match-p (term tokens)
  "Return true when TERM equals one of TOKENS or prefixes one with four or more characters."
  (and (some (lambda (token)
               (or (string= term token)
                   (and (>= (length term) 4)
                        (eql 0 (search term token)))))
             tokens)
       t))

(defun tool-search--score (namespace-name tool terms)
  "Return how strongly TOOL in NAMESPACE-NAME matches TERMS, zero for no match."
  (let ((name (or (json-get tool "name") ""))
        (name-tokens (tool-search--tokens (json-get tool "name")))
        (namespace-tokens (tool-search--tokens namespace-name))
        (description-tokens (tool-search--tokens (json-get tool "description"))))
    (loop for term in terms
          sum (cond
                ((string-equal term name)
                 5)
                ((tool-search--token-match-p term name-tokens)
                 3)
                ((tool-search--token-match-p term namespace-tokens)
                 2)
                ((tool-search--token-match-p term description-tokens)
                 1)
                (t
                 0)))))

(defun tool-search--replay-namespace (entry)
  "Return namespace ENTRY rebuilt in the deferred wire shape, or NIL to drop it."
  (when (and (json-object-p entry)
             (json-string= (json-get entry "type") "namespace")
             (non-empty-string-p (json-get entry "name")))
    (let ((tools (json-get entry "tools")))
      (json-object
       "type" "namespace"
       "name" (json-get entry "name")
       "description" (or (json-get entry "description") "")
       "tools" (if (and (vectorp tools) (not (stringp tools)))
                   (coerce
                    (loop for tool across tools
                          when (and (json-object-p tool)
                                    (non-empty-string-p (json-get tool "name"))
                                    (json-object-p (json-get tool "parameters")))
                            collect (provider-deferred-namespace-tool tool))
                    'vector)
                   (json-array))))))
