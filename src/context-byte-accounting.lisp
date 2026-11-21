(in-package #:cl-llm-provider-api)

;;;; -- Context Byte Accounting --

(defun context-inserted-utf8-bytes (before after)
  "Count inserted UTF-8 bytes on a bounded Myers character diff.

Deletion-only edits receive no credit. Common prefix and suffix removal
preserves separated unchanged islands; an exhausted search charges the
unmatched replacement middle conservatively."
  (block nil
    (let* ((prefix  (or (mismatch before after) (return 0)))
           (old-end (length before))
           (new-end (length after)))
      (loop while (and (> old-end prefix) (> new-end prefix)
                       (char= (char before (1- old-end))
                              (char after (1- new-end))))
            do (decf old-end) (decf new-end))
      (let* ((old            (subseq before prefix old-end))
             (new            (subseq after prefix new-end))
             (old-count      (length old))
             (new-count      (length new))
             (sizes          (map 'vector
                                 (lambda (character)
                                   (let ((code (char-code character)))
                                    (cond
                                      ((< code #x80)
                                       1)
                                      ((< code #x800)
                                       2)
                                      ((< code #x10000)
                                       3)
                                      (t
                                       4))))
                                 new))
             (budget         (max 1000000 (* 16 (+ old-count new-count))))
             (distance-limit (min (+ old-count new-count)
                                  (isqrt (* 2 budget))))
             (origin         (1+ distance-limit))
             (positions      (make-array (+ (* 2 distance-limit) 3)
                                         :initial-element -1))
             (costs          (make-array (length positions) :initial-element 0))
             (replacement    (reduce #'+ sizes :initial-value 0)))
        (when (zerop new-count)
          (return 0))
        (when (zerop old-count)
          (return replacement))
        (setf (aref positions (1+ origin)) 0)
        (loop for distance from 0 to distance-limit do
          (loop for diagonal from (- distance) to distance by 2
                for index = (+ origin diagonal)
                for insertion-p = (or (= diagonal (- distance))
                                      (and (/= diagonal distance)
                                           (< (aref positions (1- index))
                                              (aref positions (1+ index)))))
                for x = (if insertion-p
                            (aref positions (1+ index))
                            (1+ (aref positions (1- index))))
                for y = (- x diagonal)
                for cost = (if insertion-p
                               (+ (aref costs (1+ index))
                                  (if (and (plusp distance)
                                           (<= 1 y new-count))
                                      (aref sizes (1- y))
                                      0))
                               (aref costs (1- index)))
                do (when (minusp (decf budget))
                     (return-from context-inserted-utf8-bytes replacement))
                   (loop while (and (< x old-count) (< y new-count)
                                    (char= (char old x) (char new y)))
                         do (incf x) (incf y)
                            (when (minusp (decf budget))
                              (return-from context-inserted-utf8-bytes replacement)))
                   (when (and (>= x old-count) (>= y new-count))
                     (return-from context-inserted-utf8-bytes cost))
                   (setf (aref positions index) x
                         (aref costs index) cost)))
        replacement))))
