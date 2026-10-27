(in-package #:cl-llm-provider-api/tests)

(defun dexador-tests--tls-condition (&key queue printed-queue verify-error)
  "Construct the TLS library condition saved by cl+ssl on SSL_ERROR_SSL."
  (let ((condition (make-condition 'cl+ssl::ssl-error-ssl
                                   :handle nil :ret 1
                                   :queue queue :printed-queue printed-queue)))
    (setf (cl+ssl::ssl-error-ssl-verify-error condition) verify-error)
    condition))

(defun test-tls-transport-classification ()
  "Classify only unexpected EOF as transient at open and stream-read boundaries."
  (let* ((eof         "00000000:error:0A000126:SSL routines::unexpected eof while reading:ssl/record/example.c:1:")
         (certificate "00000000:error:0A000086:SSL routines::certificate verify failed:ssl/statem/example.c:1:")
         (handshake   "00000000:error:0A000410:SSL routines::sslv3 alert handshake failure:ssl/record/example.c:1:")
         (cases       (list
                       (list '(#x0a000126) nil nil t)
                       (list nil eof nil t)
                       (list nil (format nil "~A~C~%" (string-downcase eof) #\Return) nil t)
                       (list nil (format nil "~A~%~A~%" eof eof) nil t)
                       (list '(#x0a000086) nil nil nil)
                       (list nil certificate nil nil)
                       (list nil handshake nil nil)
                       (list '(#x05800126) nil nil nil)
                       (list nil "00000000:error:05800126:x509 certificate routines::other reason:" nil nil)
                       (list nil "unexpected eof while reading" nil nil)
                       (list nil nil nil nil)
                       (list '(#x0a000126 #x0a000086) eof nil nil)
                       (list nil (format nil "~A~%~A~%" eof certificate) nil nil)
                       (list nil eof "certificate verification failed" nil))))
    (dolist (case cases)
      (destructuring-bind (queue printed verify-error retryable-p) case
        (dolist (terminal-errors-p '(nil t))
          (let* ((raw (dexador-tests--tls-condition
                       :queue queue :printed-queue printed :verify-error verify-error))
                 (failure
                   (handler-case
                       (cl-llm-provider-api::provider--normalize-transport-operation
                        (lambda () (error raw)) :terminal-errors-p terminal-errors-p)
                     (provider-error (condition) condition))))
            (check (eq (typep failure 'provider-transport-error) retryable-p)
                   "TLS queue ~S/~S has incorrect transport classification" queue printed)
            (check (eq (typep failure 'provider-retryable-error) retryable-p)
                   "TLS queue ~S/~S has incorrect retry classification" queue printed)))))))

(defun test-tls-eof-reconnect ()
  "Recover from a saved TLS EOF through the existing streaming retry ladder."
  (dolist (output-p '(nil t))
    (let ((attempts 0)
          (events   nil))
      (check
       (eq :ok
           (call-with-streaming-retries
            (lambda (callback)
              (incf attempts)
              (when output-p
                (funcall callback (make-instance 'assistant-delta-event :text "partial")))
              (cl-llm-provider-api::provider--call-with-transport-normalization
               (lambda ()
                 (if (= attempts 1)
                     (funcall cl-llm-provider-api::*provider-transport-operation-wrapper*
                              (lambda ()
                                (error (dexador-tests--tls-condition
                                        :printed-queue "00000000:error:0A000126:SSL routines::unexpected eof while reading:"))))
                     :ok))))
            (lambda (event) (push event events))
            :sleep-function #'retry-tests--no-sleep))
       "a TLS EOF reconnects before or after streamed output")
      (let ((failures (remove-if-not
                       (lambda (event) (typep event 'provider-attempt-failed-event))
                       events)))
        (check (and (= attempts 2)
                    (= (length failures) 1)
                    (provider-attempt-failed-event-retryable-p (first failures))
                    (eq (provider-attempt-failed-event-output-received-p (first failures)) output-p))
               "the TLS EOF is recorded as one retryable failed attempt")))))

(defun run-dexador-tests ()
  "Run the TLS transport classification and reconnect checks."
  (test-tls-transport-classification)
  (test-tls-eof-reconnect))
