(in-package #:cl-llm-provider-api)


(defun provider-signal-http-failure (provider condition)
  "Record CONDITION headers and signal a typed provider or authentication error."
  (let ((status (dexador.error:response-status condition))
        (headers
         (provider--sanitize-wire-value (dexador.error:response-headers condition)))
        (body
         (let ((text
                (provider--error-body-text
                 (handler-case (dexador.error:response-body condition)
                               (error nil nil)))))
           (and text (provider--sanitize-wire-string text)))))
    (provider-note-response-headers provider headers)
    (if (= status 401)
        (error 'provider-unauthorized :message
               (format nil "The provider rejected the current ~A credentials."
                       (provider-account-label provider))
               :status status :request-id (provider--response-request-id headers)
               :response nil)
        (error
         (if (provider-retryable-status-p provider status headers)
             'provider-retryable-error
             'provider-error)
         :message (provider--http-error-message status body) :status status :request-id
         (provider--response-request-id headers) :response
         (and body (bounded-string body :limit 2000))))))

(defun provider-post-event-stream
    (url body &key headers keep-alive-p (deadline-seconds 300)
                   (connect-timeout 30) (read-timeout 300))
  "POST the encoded request BODY to URL and return Dexador's streaming response values.

BODY is the request already encoded, as a string or octet vector, so the host
keeps its own JSON encoder. The body stream, status, and headers come back as
DEXADOR:POST returns them, for PROVIDER-OPEN-RESPONSE-STREAM methods. The
response must begin within DEADLINE-SECONDS. KEEP-ALIVE-P keeps the connection
in Dexador's pool, so the requests of one conversation reach the same backend;
it is off by default."
  (provider-call-with-response-deadline
   deadline-seconds
   (lambda ()
     (dexador:post url
                   :headers headers
                   :content body
                   :want-stream t
                   :force-string t
                   :keep-alive keep-alive-p
                   :use-connection-pool keep-alive-p
                   :connect-timeout connect-timeout
                   :read-timeout read-timeout))))

(defun provider--call-with-transport-normalization
    (attempt-function &key terminal-errors-p)
  "Normalize transport operations during ATTEMPT-FUNCTION, not its callbacks.
Bind the adapter used by PROVIDER-EXECUTE-REQUEST and SSE line reads. With
TERMINAL-ERRORS-P, ATTEMPT-FUNCTION itself must be a transport-opening operation;
raw SIMPLE-ERROR failures at that boundary become terminal provider errors."
  (if terminal-errors-p
      (provider--normalize-transport-operation attempt-function :terminal-errors-p t)
      (let ((*provider-transport-operation-wrapper*
              #'provider--normalize-transport-operation))
        (funcall attempt-function))))

(defun provider--normalize-transport-operation (function &key terminal-errors-p)
  "Normalize dependency conditions from one transport FUNCTION."
  (handler-case (funcall function)
                (sb-sys:deadline-timeout (condition)
                 (provider--signal-transport-failure
                  (provider--transport-failure-message
                   "The provider response exceeded its deadline." condition)
                  :retryable-p t))
                (cl+ssl::ssl-error-syscall (condition)
                 (provider--signal-transport-failure
                  (provider--transport-failure-message
                   "The provider connection failed before the request completed."
                   condition)
                  :retryable-p t))
                (usocket:socket-error (condition)
                 (provider--signal-transport-failure
                  (provider--transport-failure-message
                   "The provider connection failed before the request completed."
                   condition)
                  :retryable-p t))
                (sb-bsd-sockets:name-service-error (condition)
                                                   (provider--signal-transport-failure
                                                    (provider--transport-failure-message
                                                     "The provider address could not be resolved."
                                                     condition)
                                                    :retryable-p t))
                (usocket:ns-error (condition)
                 (provider--signal-transport-failure
                  (provider--transport-failure-message
                   "The provider address could not be resolved." condition)
                  :retryable-p t))
                (cl+ssl::cl+ssl-error (condition)
                 (provider--signal-transport-failure
                  (provider--transport-failure-message
                   "The provider TLS connection could not be established." condition)
                  :retryable-p nil))
                (simple-error (condition)
                  (if terminal-errors-p
                      (provider--signal-transport-failure
                       (provider--transport-failure-message
                        "The provider transport failed before a response was received."
                        condition)
                       :retryable-p nil)
                      (error condition)))))


;;;; -- SSE Inactivity Deadline --

(defparameter *sse-inactivity-seconds* 300
  "Seconds one provider stream line may stall before the read fails, or NIL for no bound.

Dexador's :READ-TIMEOUT governs the response header exchange but not the
blocking reads that follow on a TLS stream, so a connection lost mid-stream
otherwise parks the request forever.")

(defun sse-read-line-within-inactivity-deadline (stream)
  "Read one bounded SSE line, failing when STREAM stalls past *SSE-INACTIVITY-SECONDS*.

The deadline covers one line, so every delivered line renews it. A stall
signals a retryable RESPONSE-STREAM-ERROR the bounded retries reconnect from.
Install it as *SSE-READ-LINE-FUNCTION*."
  (let ((seconds *sse-inactivity-seconds*))
    (if (and seconds (plusp seconds))
        (handler-case
            (provider-call-with-response-deadline
             seconds (lambda () (sse-read-line-characters stream)))
          (sb-sys:deadline-timeout ()
            (error 'response-stream-error
                   :message (format nil "The provider stream delivered nothing for ~D seconds."
                                    seconds)
                   :status nil
                   :request-id nil
                   :response nil)))
        (sse-read-line-characters stream))))

