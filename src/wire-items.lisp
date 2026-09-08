(in-package #:cl-llm-provider-api)


(defun responses-developer-message (instructions)
  "Return a standard Responses developer message containing INSTRUCTIONS."
  (json-object "type" "message" "role" "developer" "content"
   (json-array (json-object "type" "input_text" "text" instructions))))

(defun responses-standard-instructions (parts)
  "Join non-empty instruction PARTS for a standard Responses request."
  (format nil "~{~A~^~2%~}" (remove-if-not #'non-empty-string-p parts)))

(defun response-item-assistant-text (item)
  "Return the joined visible text of assistant message ITEM, when applicable."
  (when
      (and (json-string= (json-get item "type") "message")
           (json-string= (json-get item "role") "assistant"))
    (let ((content (json-get item "content")))
      (when (vectorp content)
        (let ((parts
               (loop for part across content
                     when (and (json-object-p part)
                               (json-string-member-p (json-get part "type")
                                '("output_text" "text"))
                               (stringp (json-get part "text")))
                     collect (json-get part "text"))))
          (when parts (format nil "~{~A~^~%~}" parts)))))))

(defun provider-result-assistant-text (result)
  "Return the joined assistant text across RESULT's output items."
  (let ((parts
         (loop for item in (provider-result-output-items result)
               for text = (and (json-object-p item)
                               (response-item-assistant-text item))
               when text
               collect text)))
    (when parts (format nil "~{~A~^~%~}" parts))))

(defun response-item-reasoning-summary (item)
  "Return ITEM's provider-visible reasoning summary, never raw reasoning text."
  (when (json-string= (json-get item "type") "reasoning")
    (let ((summary (json-get item "summary")))
      (when (vectorp summary)
        (let ((parts
               (loop for part across summary
                     when (and (json-object-p part)
                               (json-string= (json-get part "type") "summary_text")
                               (non-empty-string-p (json-get part "text")))
                     collect (json-get part "text"))))
          (when parts (format nil "~{~A~^~2%~}" parts)))))))
