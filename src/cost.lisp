(in-package #:cl-llm-provider-api)

;;;; -- Monetary Cost --

;;; Prices turn portable token usage into money, apart from the token budgets
;;; of inference runs. Rates are amounts per million tokens in one currency.
;;; A provider-reported charge is preferred over an estimate. Usage that
;;; cannot be priced stays unknown instead of counting as free.

(define-condition cost-limit-reached (error)
  ((limit
    :initarg       :limit
    :reader        cost-limit-reached-limit
    :type          (real 0)
    :documentation "The spending limit that was reached.")
   (spent
    :initarg       :spent
    :reader        cost-limit-reached-spent
    :type          (real 0)
    :documentation "The amount recorded when the limit was checked.")
   (currency
    :initarg       :currency
    :reader        cost-limit-reached-currency
    :type          string
    :documentation "The currency of LIMIT and SPENT."))
  (:documentation "Recorded spending reached a configured limit.")
  (:report
   (lambda (condition stream)
     (format stream "The ~,2F ~A cost limit is reached: ~,4F ~A recorded."
             (float (cost-limit-reached-limit condition) 1d0)
             (cost-limit-reached-currency condition)
             (float (cost-limit-reached-spent condition) 1d0)
             (cost-limit-reached-currency condition)))))

(defclass model-price ()
  ((currency
    :initarg       :currency
    :reader        model-price-currency
    :type          string
    :documentation "The currency of every rate.")
   (input
    :initarg       :input
    :reader        model-price-input
    :type          rational
    :documentation "The rate for uncached input tokens, per million.")
   (cache-read
    :initarg       :cache-read
    :reader        model-price-cache-read
    :type          (or null rational)
    :documentation "The rate for prompt-cache reads per million, or NIL to charge them as input.")
   (cache-write
    :initarg       :cache-write
    :reader        model-price-cache-write
    :type          (or null rational)
    :documentation "The rate for prompt-cache writes per million, or NIL to charge them as input.")
   (output
    :initarg       :output
    :reader        model-price-output
    :type          rational
    :documentation "The rate for output tokens, reasoning included, per million.")
   (source
    :initarg       :source
    :reader        model-price-source
    :type          (or null string)
    :documentation "Where and when the rates were taken, kept with every estimate."))
  (:documentation "A snapshot of one model's token prices."))

(defclass cost-ledger ()
  ((lock
    :initform      (bordeaux-threads:make-lock "Cost ledger")
    :reader        cost-ledger--lock
    :documentation "The lock serializing records from concurrent requests.")
   (totals
    :initform      (make-hash-table :test #'equal)
    :reader        cost-ledger--totals
    :type          hash-table
    :documentation "The recorded amount per currency.")
   (counts
    :initform      (list :reported 0 :estimated 0 :unpriced 0 :unknown 0)
    :accessor      cost-ledger--counts
    :type          list
    :documentation "The number of requests recorded on each basis.")
   (entries
    :initform      nil
    :accessor      cost-ledger--entries
    :type          list
    :documentation "The newest recorded entries, newest first.")
   (entry-limit
    :initarg       :entry-limit
    :reader        cost-ledger--entry-limit
    :type          (integer 0)
    :documentation "The most entries kept for inspection; totals cover every entry."))
  (:documentation "A thread-safe record of what completed requests cost."))


;;;; -- Prices --

(defun make-model-price (&key (currency "USD") input cache-read cache-write output source)
  "Return a price snapshot with rates per million tokens in CURRENCY.

INPUT and OUTPUT are required. CACHE-READ and CACHE-WRITE default to the input
rate. Decimal rates are kept as the exact fractions they name. SOURCE says
where and when the rates were taken."
  (flet ((rate (value name &key optional-p)
           (cond
             ((and optional-p (null value))
              nil)
             ((and (realp value) (>= value 0))
              (rationalize value))
             (t
              (error "A model price ~A rate must be a nonnegative real, not ~S." name value)))))
    (unless (and (stringp currency) (plusp (length currency)))
      (error "A model price needs a currency name, not ~S." currency))
    (make-instance 'model-price
                   :currency    currency
                   :input       (rate input "input")
                   :cache-read  (rate cache-read "cache read" :optional-p t)
                   :cache-write (rate cache-write "cache write" :optional-p t)
                   :output      (rate output "output")
                   :source      source)))

(defun usage-token-categories (usage)
  "Return USAGE's billed token categories, or NIL when input or output is unknown.

USAGE is portable usage, as a string-keyed hash table or (NAME VALUE) entries,
whose input_tokens include cache reads and writes. The result is a property
list of :INPUT (uncached input), :CACHE-READ, :CACHE-WRITE and :OUTPUT."
  (let ((input  (usage--field usage "input_tokens"))
        (output (usage--field usage "output_tokens"))
        (read   (or (usage--field usage "cached_input_tokens") 0))
        (write  (or (usage--field usage "cache_creation_input_tokens") 0)))
    (and input output
         (list :input       (max 0 (- input read write))
               :cache-read  read
               :cache-write write
               :output      output))))

(defun usage-price-amount (usage price)
  "Return the estimated cost of USAGE under PRICE as an exact amount, or NIL.

NIL means USAGE lacks input or output counts, so its cost is unknown."
  (let ((tokens (usage-token-categories usage)))
    (and tokens
         (/ (+ (* (getf tokens :input) (model-price-input price))
               (* (getf tokens :cache-read)
                  (or (model-price-cache-read price) (model-price-input price)))
               (* (getf tokens :cache-write)
                  (or (model-price-cache-write price) (model-price-input price)))
               (* (getf tokens :output) (model-price-output price)))
            1000000))))

(defun usage-reported-cost (usage)
  "Return the charge a provider reported in USAGE and its currency, or NIL.

OpenRouter reports what a request cost in US dollars as usage's cost field."
  (let ((cost (usage--field usage "cost" :type 'real)))
    (if (and cost (>= cost 0))
        (values (rationalize cost) "USD")
        (values nil nil))))


;;;; -- Ledger --

(defun make-cost-ledger (&key (entry-limit 256))
  "Return an empty cost ledger keeping the newest ENTRY-LIMIT entries for inspection."
  (make-instance 'cost-ledger :entry-limit entry-limit))

(defun cost-ledger-record (ledger usage &key price model route)
  "Record one completed request's USAGE in LEDGER and return its entry.

The amount is the charge USAGE reports, else the estimate under PRICE. The
entry's :BASIS is :REPORTED, :ESTIMATED, :UNPRICED when there is no PRICE, or
:UNKNOWN when USAGE lacks the counts to price. Unpriced and unknown requests
add nothing to the totals and are counted apart, as unknown spending. MODEL
and ROUTE identify what served the request."
  (multiple-value-bind (reported reported-currency) (usage-reported-cost usage)
    (let* ((tokens   (usage-token-categories usage))
           (estimate (and price tokens (usage-price-amount usage price)))
           (basis    (cond
                       (reported
                        ':reported)
                       (estimate
                        ':estimated)
                       ((null tokens)
                        ':unknown)
                       (t
                        ':unpriced)))
           (currency (cond
                       (reported
                        reported-currency)
                       (estimate
                        (model-price-currency price))
                       (t
                        nil)))
           (amount   (or reported estimate))
           (entry    (list :model        model
                           :route        route
                           :basis        basis
                           :currency     currency
                           :amount       amount
                           :tokens       tokens
                           :price-source (and price (model-price-source price)))))
      (bordeaux-threads:with-lock-held ((cost-ledger--lock ledger))
        (when amount
          (incf (gethash currency (cost-ledger--totals ledger) 0) amount))
        (incf (getf (cost-ledger--counts ledger) basis))
        (push entry (cost-ledger--entries ledger))
        (let ((limit (cost-ledger--entry-limit ledger)))
          (when (> (length (cost-ledger--entries ledger)) limit)
            (setf (cost-ledger--entries ledger)
                  (subseq (cost-ledger--entries ledger) 0 limit)))))
      entry)))

(defun cost-ledger-total (ledger &key (currency "USD"))
  "Return the amount LEDGER recorded in CURRENCY."
  (bordeaux-threads:with-lock-held ((cost-ledger--lock ledger))
    (gethash currency (cost-ledger--totals ledger) 0)))

(defun cost-ledger-summary (ledger)
  "Return LEDGER's totals and request counts as a property list.

:TOTALS lists (CURRENCY AMOUNT) pairs sorted by currency. :COUNTS gives the
number of requests on each basis, so unpriced and unknown spending stays
visible next to the totals."
  (bordeaux-threads:with-lock-held ((cost-ledger--lock ledger))
    (list :totals (sort (loop for currency being the hash-keys of (cost-ledger--totals ledger)
                                using (hash-value amount)
                              collect (list currency amount))
                        #'string< :key #'first)
          :counts (copy-list (cost-ledger--counts ledger)))))

(defun cost-ledger-entries (ledger)
  "Return the entries LEDGER keeps, newest first."
  (bordeaux-threads:with-lock-held ((cost-ledger--lock ledger))
    (copy-list (cost-ledger--entries ledger))))

(defun cost-ledger-check-limit (ledger limit &key (currency "USD"))
  "Signal COST-LIMIT-REACHED when LEDGER's CURRENCY total reached LIMIT.

A NIL LIMIT checks nothing. Return NIL otherwise."
  (when limit
    (let ((spent (cost-ledger-total ledger :currency currency)))
      (when (>= spent limit)
        (error 'cost-limit-reached :limit limit :spent spent :currency currency))))
  nil)


;;;; -- Private Functions --

(defun usage--field (usage name &key (type '(integer 0)))
  "Return USAGE's NAME value when it is of TYPE, reading a hash table or entries."
  (let ((value (cond
                 ((hash-table-p usage)
                  (gethash name usage))
                 ((listp usage)
                  (second (assoc name usage :test #'equal)))
                 (t
                  nil))))
    (and (typep value type) value)))
