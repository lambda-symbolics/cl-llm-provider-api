(defpackage #:cl-llm-provider-api/tests
  (:use #:cl #:cl-llm-provider-api)
  (:export #:run-tests))

(in-package #:cl-llm-provider-api/tests)

(defvar *assertions* 0)

(defun check (value control &rest arguments)
  "Count one assertion and fail with CONTROL unless VALUE is true."
  (incf *assertions*)
  (unless value
    (error (apply #'format nil control arguments))))

(defclass test-responses-provider (responses-api-provider)
  ())

(defmethod provider-stream-turn
    ((provider test-responses-provider) conversation
     &key tool-namespaces event-callback goal-context compaction-p)
  (declare (ignore provider tool-namespaces goal-context compaction-p))
  (when event-callback
    (funcall event-callback
             (make-instance 'assistant-delta-event :text "hello")))
  (make-instance 'provider-result
                 :response-id "response-1"
                 :output-items (list conversation)
                 :usage '(:total-tokens 7)
                 :turn-completion :end))

(defun test-provider-values ()
  "Exercise portable provider events and results."
  (let ((delta (make-instance 'assistant-delta-event :text "part"))
        (reasoning (make-instance 'reasoning-delta-event :text "summary"))
        (item (make-instance 'provider-item-event :item '(:kind :message)))
        (completed (make-instance 'provider-completed-event
                                  :response-id "r"
                                  :usage '(:tokens 4)
                                  :turn-completion :continue))
        (retry (make-instance 'provider-retry-event
                              :attempt 2
                              :maximum-attempts 4
                              :delay 0.5)))
    (check (typep delta 'provider-event) "delta is not a provider event")
    (check (string= (assistant-delta-event-text delta) "part")
           "assistant delta lost its text")
    (check (string= (reasoning-delta-event-text reasoning) "summary")
           "reasoning delta lost its text")
    (check (equal (provider-item-event-item item) '(:kind :message))
           "item event lost its value")
    (check (and (string= (provider-completed-event-response-id completed) "r")
                (equal (provider-completed-event-usage completed) '(:tokens 4))
                (eq (provider-completed-event-turn-completion completed) :continue))
           "completed event lost terminal metadata")
    (check (and (= (provider-retry-event-attempt retry) 2)
                (= (provider-retry-event-maximum-attempts retry) 4)
                (= (provider-retry-event-delay retry) 0.5))
           "retry event lost reconnect metadata")))

(defun test-provider-protocol ()
  "Exercise generic provider and wire protocol defaults."
  (let* ((provider (make-instance 'test-responses-provider
                                  :registration '(:name "test")
                                  :configuration '(:model "test-model")
                                  :credential-manager ':test-credentials
                                  :session-id "session-1"))
         (events nil)
         (result (provider-stream-turn
                  provider '(:role :user)
                  :event-callback (lambda (event) (push event events)))))
     (check (eq (provider-family provider) :custom)
            "provider family default is not custom")
     (check (equal (model-provider-registration provider) '(:name "test"))
            "provider lost its opaque registration")
     (check (equal (provider-configuration provider) '(:model "test-model"))
            "subscription provider lost its configuration")
     (check (eq (provider-credential-manager provider) :test-credentials)
            "subscription provider lost its credential manager")
     (check (string= (provider-session-id provider) "session-1")
            "subscription provider lost its session identity")
    (check (eq (provider-wire-protocol provider) :responses-api)
           "Responses provider has the wrong wire family")
    (check (string= (provider-wire-tool-name provider "resource" "read")
                    "resource.read")
           "Responses tool names are not flattened")
    (check (equal (provider-result-output-items result) '((:role :user)))
           "provider result lost output items")
    (check (equal (provider-result-usage result) '(:total-tokens 7))
           "provider result lost usage")
    (check (eq (provider-result-turn-completion result) :end)
           "provider result lost completion state")
    (check (and (= (length events) 1)
                (string= (assistant-delta-event-text (first events)) "hello"))
           "stream callback did not receive the semantic event")
    (check (null (provider-native-compact-conversation provider nil))
           "native compaction default is not NIL")
    (check (null (provider-output-ceiling-p provider))
           "output ceiling default is not conservative")
    (check (eq (provider-set-reasoning-summaries provider t) provider)
           "reasoning summary default did not preserve provider")
    (check (eq (provider-normalize-output-item provider result) result)
           "normalization default changed the item")
    (check (equalp (provider-responses-request-namespaces provider #(a b)) #(a b))
           "Responses namespace default changed input")
    (check (null (provider-responses-hosted-tools provider nil))
           "hosted tools default is not NIL")
    (check (null (provider-responses-reasoning-summary provider nil))
           "reasoning summary style default is not NIL")
    (check (null (provider-responses-request-fields provider nil))
           "Responses request fields default is not NIL")
    (check (null (provider-responses-request-fields
                  provider nil :compaction-p t))
           "Responses request fields rejected compaction context")
    (check (eq (provider-responses-instructions-placement provider) :input)
           "Responses instructions placement default is not input")))

(defun test-inference-budget ()
  "Exercise shared call, token, and depth accounting."
  (let ((budget (rlm-budget-create :calls 2 :tokens 100 :depth 1)))
    (check (= (rlm-budget-remaining-calls budget) 2)
           "fresh budget lost calls")
    (let ((tranche (rlm-budget-acquire-request budget)))
      (check (= tranche 25) "reservation did not take a bounded share")
      (check (= (rlm-budget-remaining-tokens budget) 75)
             "reservation did not leave the expected token balance")
      (rlm-budget-settle-output budget tranche 40)
      (check (= (rlm-budget-remaining-tokens budget) 60)
             "settlement did not charge actual usage"))
    (let ((tranche (rlm-budget-acquire-request budget)))
      (check (= tranche 16) "small reservation missed the provider floor")
      (rlm-budget-settle-output budget tranche nil)
      (check (= (rlm-budget-remaining-tokens budget) 60)
             "unknown usage did not refund its reservation"))
    (check (handler-case
               (progn
                 (rlm-budget-acquire-request budget :task "again")
                 nil)
             (rlm-budget-exhausted (condition)
               (and (eq (rlm-budget-exhausted-dimension condition) :calls)
                    (string= (rlm-budget-exhausted-task condition) "again"))))
           "drained call budget did not identify its dimension and task"))
  (let* ((root (rlm-budget-create :calls 2 :tokens 50 :depth 1))
         (child (rlm-budget-descend root :task "child")))
    (check (zerop (rlm-budget-remaining-depth child))
           "descended budget did not spend depth")
    (rlm-budget-acquire-request child)
    (check (= (rlm-budget-remaining-calls root) 1)
           "descended budget did not share the call pool")
    (check (handler-case
               (progn (rlm-budget-descend child) nil)
             (rlm-budget-exhausted (condition)
               (eq (rlm-budget-exhausted-dimension condition) :depth)))
           "zero-depth budget allowed another descent")))

(defun test-inference-budget-contention ()
  "Exercise atomic reservations under concurrent acquisition."
  (let* ((budget (rlm-budget-create :calls 16 :tokens 100 :depth 1))
         (lock (bordeaux-threads:make-lock "budget test results"))
         (tranches nil)
         (refusals 0))
    (mapc #'bordeaux-threads:join-thread
          (loop repeat 8
                collect
                (bordeaux-threads:make-thread
                 (lambda ()
                   (handler-case
                       (let ((tranche (rlm-budget-acquire-request budget)))
                         (bordeaux-threads:with-lock-held (lock)
                           (push tranche tranches)))
                     (rlm-budget-exhausted ()
                       (bordeaux-threads:with-lock-held (lock)
                         (incf refusals))))))))
    (check (= (+ (length tranches) refusals) 8)
           "a competing reservation disappeared")
    (check (<= (reduce #'+ tranches :initial-value 0) 100)
           "concurrent reservations oversubscribed the token pool")
    (check (plusp (length tranches))
           "every concurrent reservation was refused")))

(defun test-inference-views ()
  "Exercise view materialization, provenance, and rendering."
  (let ((views (rlm-views-materialize
                (list "first literal"
                      (list :label "notes" :content "second literal")))))
    (check (equal (mapcar #'rlm-view-label views) '("literal" "notes"))
           "view labels changed during materialization")
    (check (string= (rlm-view-digest (first views))
                    (rlm-content-digest "first literal"))
           "view digest does not match its content")
    (let ((rendered (rlm-views-render views)))
      (check (search "label=\"notes\"" rendered)
             "rendered views omitted their label")
      (check (search "second literal" rendered)
             "rendered views omitted exact content")
      (check (search (subseq (rlm-view-digest (second views)) 0 12) rendered)
             "rendered views omitted digest provenance")))
  (check (equal (mapcar #'rlm-view-label
                        (rlm-views-materialize (list "one" "two")))
                '("literal#1" "literal#2"))
         "duplicate view labels were not numbered"))

(defun call-with-temporary-directory (function)
  "Call FUNCTION with a fresh directory and remove it afterwards."
  (let* ((base (uiop:temporary-directory))
         (directory (merge-pathnames
                     (format nil "cl-llm-provider-api-~36R/" (random (expt 36 8)))
                     base)))
    (ensure-directories-exist directory)
    (unwind-protect
         (funcall function directory)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defun test-inference-objects ()
  "Exercise content-addressed context storage and integrity checks."
  (call-with-temporary-directory
   (lambda (directory)
     (let* ((store (rlm-context-store-create directory))
            (first (rlm-context-intern store "durable context" :label "notes"))
            (second (rlm-context-intern store "durable context")))
       (check (string= (rlm-context-object-digest first)
                       (rlm-context-object-digest second))
              "identical context did not share an object identity")
       (check (equal (rlm-context-object-pathname first)
                     (rlm-context-object-pathname second))
              "identical context did not share an object pathname")
       (check (string= (rlm-context-object-content first) "durable context")
              "context object did not return its content")
       (multiple-value-bind (found content)
           (rlm-context-object-find store (rlm-context-object-digest first))
         (check (and found (string= content "durable context"))
                "context lookup did not return the verified content"))
       (check (handler-case
                  (progn (rlm-context-object-find store "../outside") nil)
                (rlm-view-error () t))
              "invalid object digest was accepted as a pathname")
       (with-open-file (stream (rlm-context-object-pathname first)
                               :direction :output
                               :if-exists :supersede)
         (write-string "corrupt" stream))
       (check (handler-case
                  (progn (rlm-context-object-content first) nil)
                (rlm-view-error () t))
              "corrupt context content passed integrity verification")
       (let ((repaired (rlm-context-intern store "durable context")))
         (check (string= (rlm-context-object-content repaired) "durable context")
                "reintern did not repair corrupt content"))))))

(defun test-inference-object-paths ()
  "Exercise absolute store identity and cache pathname confinement."
  (call-with-temporary-directory
   (lambda (directory)
     (let* ((*default-pathname-defaults* directory)
            (store (rlm-context-store-create #P"objects/"))
            (object (rlm-context-intern store "confined context"))
            (outside (merge-pathnames "outside.txt" directory)))
       (check (uiop:absolute-pathname-p (rlm-context-store-root store))
              "relative context store root was not made absolute")
       (with-open-file (stream outside
                               :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
         (write-string "confined context" stream))
       (let ((forged
               (make-instance 'rlm-context-object
                              :store store
                              :digest (rlm-context-object-digest object)
                              :label "forged"
                              :characters (length "confined context")
                              :pathname outside)))
         (check (handler-case
                    (progn (rlm-context-object-content forged) nil)
                  (rlm-view-error () t))
                "context object accepted a pathname outside its store"))
       (call-with-temporary-directory
        (lambda (other-directory)
          (let* ((other-store (rlm-context-store-create other-directory))
                 (copied (rlm-context-designator-object other-store object)))
            (check (string= (rlm-context-object-content copied)
                            "confined context")
                   "foreign context object did not retain its content")
            (check (uiop:subpathp (rlm-context-object-pathname copied)
                                  (rlm-context-store-root other-store))
                   "foreign context object was not copied into the target store"))))))))

(defun run-tests ()
  "Run every cl-llm-provider-api test."
  (setf *assertions* 0)
  (test-provider-values)
  (test-provider-protocol)
  (test-inference-budget)
  (test-inference-budget-contention)
  (test-inference-views)
  (test-inference-objects)
  (test-inference-object-paths)
  (format t "~&~D cl-llm-provider-api tests passed.~%" *assertions*)
  t)
