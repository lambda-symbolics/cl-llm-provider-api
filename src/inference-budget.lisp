(in-package #:cl-llm-provider-api)

;;;; -- Bounded Inference Budgets --

(defparameter *rlm-default-call-budget* 8
  "The default number of provider calls one inference subtree may spend.")

(defparameter *rlm-default-token-budget* 80000
  "The default combined token allowance for one inference subtree.")

(defparameter *rlm-default-depth-budget* 2
  "The default remaining recursion depth below a root inference frame.")

(defparameter *rlm-output-reserve-tokens* 16000
  "The largest output token tranche reserved per provider request.")

(defparameter *rlm-output-reserve-share* 4
  "The fraction of the remaining pool one reservation may take.")

(define-condition rlm-budget-exhausted (error)
  ((dimension
    :initarg :dimension
    :reader rlm-budget-exhausted-dimension
    :type keyword
    :documentation "The exhausted allowance: :calls, :tokens, or :depth.")
   (task
    :initarg :task
    :initform nil
    :reader rlm-budget-exhausted-task
    :type (or null string)
    :documentation "The inference task requesting the exceeded allowance."))
  (:documentation "An inference subtree spent its shared budget.")
  (:report
   (lambda (condition stream)
     (format stream "The inference ~A budget is exhausted~@[ for task ~S~]."
             (string-downcase
              (symbol-name (rlm-budget-exhausted-dimension condition)))
             (rlm-budget-exhausted-task condition)))))

(defclass rlm-budget-pool ()
  ((lock
    :initform (bordeaux-threads:make-lock "Inference budget")
    :reader rlm-budget-pool--lock
    :documentation "The lock serializing charges from concurrent frames.")
   (calls-remaining
    :initarg :calls-remaining
    :accessor rlm-budget-pool--calls-remaining
    :type (integer 0)
    :documentation "The provider calls the subtree may still spend.")
   (tokens-remaining
    :initarg :tokens-remaining
    :accessor rlm-budget-pool--tokens-remaining
    :type integer
    :documentation "The combined tokens the subtree may still spend."))
  (:documentation "Call and token counters shared by one inference subtree."))

(defclass rlm-budget ()
  ((pool
    :initarg :pool
    :reader rlm-budget--pool
    :type rlm-budget-pool
    :documentation "The subtree-shared call and token counters.")
   (depth-remaining
    :initarg :depth-remaining
    :reader rlm-budget-remaining-depth
    :type (integer 0)
    :documentation "The recursion levels still allowed below this budget."))
  (:documentation "One inference frame's view of a shared subtree budget."))

(defun rlm-budget-create
    (&key (calls *rlm-default-call-budget*)
          (tokens *rlm-default-token-budget*)
          (depth *rlm-default-depth-budget*))
  "Create a root inference budget of CALLS, TOKENS, and recursion DEPTH."
  (check-type calls (integer 1))
  (check-type tokens (integer 1))
  (check-type depth (integer 0))
  (make-instance 'rlm-budget
                 :pool (make-instance 'rlm-budget-pool
                                      :calls-remaining calls
                                      :tokens-remaining tokens)
                 :depth-remaining depth))

(defun rlm-budget-remaining-calls (budget)
  "Return the provider calls BUDGET's subtree may still spend."
  (let ((pool (rlm-budget--pool budget)))
    (bordeaux-threads:with-lock-held ((rlm-budget-pool--lock pool))
      (rlm-budget-pool--calls-remaining pool))))

(defun rlm-budget-remaining-tokens (budget)
  "Return the combined tokens BUDGET's subtree may still spend."
  (let ((pool (rlm-budget--pool budget)))
    (bordeaux-threads:with-lock-held ((rlm-budget-pool--lock pool))
      (max 0 (rlm-budget-pool--tokens-remaining pool)))))

(defun rlm-budget-acquire-request (budget &key task)
  "Atomically reserve one provider call and an output tranche from BUDGET.

Return the tranche to advertise as the request's output ceiling. Signal
RLM-BUDGET-EXHAUSTED when no calls or tokens remain. Settle the reservation
with RLM-BUDGET-SETTLE-OUTPUT once usage is known."
  (let ((pool (rlm-budget--pool budget)))
    (bordeaux-threads:with-lock-held ((rlm-budget-pool--lock pool))
      (when (zerop (rlm-budget-pool--calls-remaining pool))
        (error 'rlm-budget-exhausted :dimension :calls :task task))
      (unless (plusp (rlm-budget-pool--tokens-remaining pool))
        (error 'rlm-budget-exhausted :dimension :tokens :task task))
      (decf (rlm-budget-pool--calls-remaining pool))
      (let* ((remaining (rlm-budget-pool--tokens-remaining pool))
             (tranche
               (min remaining
                    (max 16 (min *rlm-output-reserve-tokens*
                                 (ceiling remaining
                                          *rlm-output-reserve-share*))))))
        (setf (rlm-budget-pool--tokens-remaining pool)
              (- remaining tranche))
        tranche))))

(defun rlm-budget-settle-output (budget tranche usage-total)
  "Settle one request's TRANCHE against its reported USAGE-TOTAL.

A NIL USAGE-TOTAL refunds the whole tranche. The internal balance may carry
debt below zero so later refunds cannot erase an earlier overdraft."
  (check-type tranche (integer 0))
  (check-type usage-total (or null (integer 0)))
  (let ((pool (rlm-budget--pool budget)))
    (bordeaux-threads:with-lock-held ((rlm-budget-pool--lock pool))
      (incf (rlm-budget-pool--tokens-remaining pool)
            (- tranche (or usage-total 0)))))
  budget)

(defun rlm-budget-descend (budget &key task)
  "Return BUDGET one recursion level down, sharing its call and token pool."
  (when (zerop (rlm-budget-remaining-depth budget))
    (error 'rlm-budget-exhausted :dimension :depth :task task))
  (make-instance 'rlm-budget
                 :pool (rlm-budget--pool budget)
                 :depth-remaining (1- (rlm-budget-remaining-depth budget))))
