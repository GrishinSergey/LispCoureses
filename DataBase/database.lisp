;; "E:/Files/LispProjects/Lection4/humans.csv"
;; "E:/Files/LispProjects/Lection4/machines.csv"
;; "E:/Files/LispProjects/Lection4/join.csv"

(defstruct struct-human id name surname age)
(defstruct struct-machine id auto color year)
(defstruct struct-join id id-1 id-2)
(defstruct struct-result id id-1 name surname age id-2 auto color year)

(defun split-str (string &optional (separator ";") (r nil))
  (let ((n (position separator string :from-end t :test #'(lambda (x y) (find y x :test #'string=)))))
    (if n
  (split-str (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
  (cons string r))))

(defun split (string &optional (separator ";"))
  (split-str string separator))

(defun set-human (lst)
  (make-struct-human
   :id (first lst)
   :name (second lst)
   :surname (third lst)
   :age (fourth lst)))

(defun set-machine (lst)
  (make-struct-machine
   :id (first lst)
   :auto (second lst)
   :color (third lst)
   :year (fourth lst)))

(defun set-result (id human machine)
  (make-struct-result
   :id id
   :id-1 (struct-human-id human)
   :id-2 (struct-machine-id machine)
   :name (struct-human-name human)
   :surname (struct-human-surname human)
   :age (struct-human-age human)
   :auto (struct-machine-auto machine)
   :color (struct-machine-color machine)
   :year (struct-machine-year machine)))

(defun set-join (lst)
  (make-struct-join
   :id (first lst)
   :id-1 (second lst)
   :id-2 (third lst)))

(defun get-machine-id (struct)
  (struct-machine-id struct))

(defun get-human-id (struct)
  (struct-human-id struct))

(defun get-join-id (struct)
  (struct-join-id struct))

(defun select (file-path func key-func)
  (if (probe-file file-path)
      (with-open-file (s file-path)
  (do ((tmp nil) (line (read-line s) (read-line s nil 'eof))
       (hash-table (make-hash-table :test #'equal)))
      ((eq line 'eof) hash-table)
    (setf tmp (funcall func (split line)))
    (setf (gethash (funcall key-func tmp) hash-table) tmp)))
      (format t "file not found~%")))

(defun insert (file-path data)
  (with-open-file (stream file-path :direction :output
            :if-exists :supersede)
    (maphash (lambda (key value)
         (format stream "~a;~a;~a;~a;~a;~a;~a;~a;~a~%"
           key
           (struct-result-id-1 value)
           (struct-result-name value)
           (struct-result-surname value)
           (struct-result-age value)
           (struct-result-id-2 value)
           (struct-result-auto value)
           (struct-result-color value)
           (struct-result-year value)))
       data)))

(defun comparator (value1 value2)
  (and (not (null value1)) (equalp value1 value2)))

(defun parse-csv (file-1 file-2 file-3)
  (lambda (res-file &key name surname age auto color year)
    (let ((humans (select file-1 #'set-human #'get-human-id))
    (machines (select file-2 #'set-machine #'get-machine-id))
    (join (select file-3 #'set-join #'get-join-id))
    (tmp (make-hash-table :test #'equal))
    (result (make-hash-table :test #'equal)))

      (maphash (lambda (key value)
     (setf (gethash key tmp) (set-result key
                 (gethash (struct-join-id-1 value) humans)
                 (gethash (struct-join-id-2 value) machines))))
         join)

      (maphash (lambda (k v)
     (when (comparator name (struct-result-name v))
       (setf (gethash k result) v))
     (when (comparator surname (struct-result-surname v))
       (setf (gethash k result) v))
     (when (comparator age (struct-result-age v))
       (setf (gethash k result) v))
     (when (comparator auto (struct-result-auto v))
       (setf (gethash k result) v))
     (when (comparator color (struct-result-color v))
       (setf (gethash k result) v))
     (when (comparator year (struct-result-year v))
       (setf (gethash k result) v)))
         tmp)

      (insert res-file result))))