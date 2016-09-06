(defun parse-str-to-hash-table (str)
  (do ((i 0 (1+ i)) (hash-table (make-hash-table :test #'equal)))
      ((= i (length str)) hash-table)
    (push i (gethash (char str i) hash-table))))


;; (PARSE-STR-TO-HASH-TABLE "qwe rty")
;; #<hash-table :test equal :count 7>
;; @key -> @val
;; q (0)
;; w (1)
;; e (2)
;;   (3)
;; r (4)
;; t (5)
;; y (6)
;; (PARSE-STR-TO-HASH-TABLE "hello world")
;; h (0)
;; e (1)
;; l (9 3 2)
;; o (7 4)
;;   (5)
;; w (6)
;; r (8)
;; d (10)


(defun make-str-from-hash-table (hash-table)
  (let ((acc nil) (res nil))
    (maphash (lambda (key value) (declare (ignore key)) (setf acc (append value acc))) hash-table)
    (setf acc (sort acc #'<))
    (mapcar (lambda (val)
        (maphash (lambda (key value)
       (when (member val value)
         (setf res (append res (list key)))))
           hash-table))
      acc)
    (concatenate 'string res)))

;; (MAKE-STR-FROM-HASH-TABLE (PARSE-STR-TO-HASH-TABLE "hello world"))
;; "hello world"
