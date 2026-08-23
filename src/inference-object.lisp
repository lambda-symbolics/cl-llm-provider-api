(in-package #:cl-llm-provider-api)

;;;; -- Content-Addressed Inference Objects --

(defclass rlm-context-store ()
  ((root
    :initarg :root
    :reader rlm-context-store-root
    :type pathname
    :documentation "The directory holding immutable context objects."))
  (:documentation "A content-addressed store for bounded inference context."))

(defclass rlm-context-object ()
  ((digest
    :initarg :digest
    :reader rlm-context-object-digest
    :type string
    :documentation "The SHA-256 content digest naming this object.")
   (label
    :initarg :label
    :reader rlm-context-object-label
    :type string
    :documentation "The short human-meaningful name shown to a model.")
   (characters
    :initarg :characters
    :reader rlm-context-object-characters
    :type (integer 0)
    :documentation "The exact character count of the stored content.")
   (pathname
    :initarg :pathname
    :reader rlm-context-object-pathname
    :type pathname
    :documentation "The content-addressed file holding the exact content."))
  (:documentation "One immutable content-addressed context object."))

(defun rlm-context-store-create (root)
  "Create a context object store rooted at directory ROOT."
  (let ((directory (uiop:ensure-directory-pathname root)))
    (ensure-directories-exist directory)
    (make-instance 'rlm-context-store :root directory)))

(defun rlm-context-object--digest-p (digest)
  "Return true when DIGEST is a canonical lowercase SHA-256 string."
  (and (stringp digest)
       (= (length digest) 64)
       (every (lambda (character)
                (or (digit-char-p character)
                    (find character "abcdef" :test #'char=)))
              digest)))

(defun rlm-context-object--pathname (store digest)
  "Return DIGEST's canonical pathname under STORE."
  (unless (rlm-context-object--digest-p digest)
    (error 'rlm-view-error
           :designator digest
           :message "expected a lowercase 64-character SHA-256 digest"))
  (merge-pathnames (make-pathname :name digest :type "txt")
                   (rlm-context-store-root store)))

(defun rlm-context-object--write (store pathname content)
  "Atomically publish CONTENT at PATHNAME inside STORE."
  (ensure-directories-exist pathname)
  (uiop:with-temporary-file (:pathname temporary
                             :stream stream
                             :keep t
                             :directory (rlm-context-store-root store)
                             :prefix "intern"
                             :external-format :utf-8)
    (unwind-protect
         (progn
           (write-string content stream)
           (finish-output stream)
           :close-stream
           (uiop:rename-file-overwriting-target temporary pathname))
      (when (probe-file temporary)
        (delete-file temporary)))))

(defun rlm-context-intern (store content &key label)
  "Store CONTENT once under its digest and return its object handle.

Interning identical content is idempotent. A stored object whose content no
longer matches its digest is repaired atomically from CONTENT."
  (check-type store rlm-context-store)
  (check-type content string)
  (let* ((digest (rlm-content-digest content))
         (pathname (rlm-context-object--pathname store digest)))
    (unless (and (probe-file pathname)
                 (string= content (uiop:read-file-string pathname)))
      (rlm-context-object--write store pathname content))
    (make-instance 'rlm-context-object
                   :digest digest
                   :label (or label "context")
                   :characters (length content)
                   :pathname pathname)))

(defun rlm-context-intern-pathname (store pathname &key label)
  "Intern the file at PATHNAME, defaulting the label to its name."
  (handler-case
      (rlm-context-intern store
                          (uiop:read-file-string pathname)
                          :label (or label (file-namestring pathname)))
    (error (condition)
      (error 'rlm-view-error
             :designator pathname
             :message (format nil "~A" condition)))))

(defun rlm-context-object-find (store digest)
  "Return the verified object for DIGEST and its content, or NIL when absent."
  (let ((pathname (rlm-context-object--pathname store digest)))
    (when (probe-file pathname)
      (let ((content (uiop:read-file-string pathname)))
        (unless (string= digest (rlm-content-digest content))
          (error 'rlm-view-error
                 :designator digest
                 :message "the stored context object no longer matches its digest"))
        (values
         (make-instance 'rlm-context-object
                        :digest digest
                        :label "context"
                        :characters (length content)
                        :pathname pathname)
         content)))))

(defun rlm-context-object-content (object)
  "Return OBJECT's content after verifying its digest."
  (let ((content (uiop:read-file-string (rlm-context-object-pathname object))))
    (unless (string= (rlm-context-object-digest object)
                     (rlm-content-digest content))
      (error 'rlm-view-error
             :designator object
             :message "the context object no longer matches its digest"))
    content))

(defun rlm-context-designator-object (store designator)
  "Intern one context DESIGNATOR: an object, string, pathname, or plist."
  (typecase designator
    (rlm-context-object designator)
    (string (rlm-context-intern store designator))
    (pathname (rlm-context-intern-pathname store designator))
    (cons
     (let ((label (getf designator :label))
           (content (getf designator :content))
           (path (getf designator :path)))
       (cond
         ((and (stringp content) (not path))
          (rlm-context-intern store content :label label))
         ((and (pathnamep path) (not content))
          (rlm-context-intern-pathname store path :label label))
         (t
          (error 'rlm-view-error
                 :designator designator
                 :message "expected :label with exactly one of :content or :path")))))
    (t
     (error 'rlm-view-error
            :designator designator
            :message "expected a context object, string, pathname, or plist"))))
