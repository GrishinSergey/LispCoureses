;; struct of node of tree
(defstruct node val children)

(defun get-tree (expression)
  "Make tree from LISP-expression"
  (if (listp expression)
      (make-node :val (car expression) :children (mapcar #'get-tree (cdr expression)))
      (make-node :val expression :children nil)))

;; (create tree '(+ 1 2 (* 3 4 5) (- 10 2)))
;; #S(NODE
;;    :VALUE +
;;    :CHILDREN (#S(NODE :VALUE 1 :CHILDREN NIL)
;;               #S(NODE :VALUE 2 :CHILDREN NIL)
;;               #S(NODE
;;                  :VALUE *
;;                  :CHILDREN (#S(NODE :VALUE 3 :CHILDREN NIL)
;;                             #S(NODE :VALUE 4 :CHILDREN NIL)
;;                             #S(NODE :VALUE 5 :CHILDREN NIL)))
;;               #S(NODE
;;                  :VALUE -
;;                  :CHILDREN (#S(NODE :VALUE 10 :CHILDREN NIL)
;;                             #S(NODE :VALUE 2 :CHILDREN NIL)))))

(defun print-expresion (tree)
  "take tree and print it as LISP-expression"
  (print (expression tree)))

;; (print-expresion (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; (+ 1 2 (* 3 4 5) (- 10 2))

(defun expression (tree)
  "make LISP-expression from tree"
  (if (numberp (node-val tree))
      (node-val tree)
      (cons (node-val tree) (mapcar #'expression (node-children tree)))))

;; (expression (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; (+ 1 2 (* 3 4 5) (- 10 2))

(defun pre-order (tree)
  (print (node-val tree))
  (mapcar #'pre-order (node-children tree)))

;; (pre-order (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; + 1 2 * 3 4 5 - 10 2

(defun post-order (tree)
  (mapcar #'post-order (node-children tree))
  (print (node-val tree)))

;; (post-order (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; 1 2 3 4 5 * 10 2 - +

(defun breadth-first (tree)
  (labels ((%bf (queue)
             (when queue
               (print (node-val (car queue)))
               (%bf (append (cdr queue) (node-children (car queue)))))))
    (%bf (list tree))))

;; (breadth-first (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; + 1 2 * - 3 4 5 10 2

(defun callback (tree-node-value)
  "use this function as lambda in map-tree. If the number decreases by one, otherwise not changing"
  (if (numberp tree-node-value)
      (1- tree-node-value)
      tree-node-value))

;; (callback 1)
;; 0
;;
;; (callback +)
;; +

(defun map-tree (tree func)
  "take a tree and a function. Func change val of node and as a result of map-tree is new tree with changed nodes"
  (make-node
   :val (funcall func (node-val tree))
   :children (mapcar (lambda (tree-node) (map-tree tree-node func))
                     (node-children tree))))

;; (map-tree ((create-tree '(+ 1 2 (* 3 4 5) (- 10 2)))) #'callback)
;; #S(NODE
;;    :VALUE +
;;    :CHILDREN (#S(NODE :VALUE 0 :CHILDREN NIL)
;;               #S(NODE :VALUE 1 :CHILDREN NIL)
;;               #S(NODE
;;                  :VALUE *
;;                  :CHILDREN (#S(NODE :VALUE 2 :CHILDREN NIL)
;;                             #S(NODE :VALUE 3 :CHILDREN NIL)
;;                             #S(NODE :VALUE 4 :CHILDREN NIL)))
;;               #S(NODE
;;                  :VALUE -
;;                  :CHILDREN (#S(NODE :VALUE 9 :CHILDREN NIL)
;;                             #S(NODE :VALUE 1 :CHILDREN NIL)))))

(defun evaluate (tree)
  "evaluate tree and return result of evaluating"
  (eval (expression tree)))

;; (evaluate (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; 71
;;
;; (evaluate (get-tree nil))
;; Error because nil is not a function

(defun eval-with-letters (tree)
  "evaluate as much as possible: if number in node try eval it, if letter in node -- don't try eval it, but eval other nodes.
As the result is new tree where some nodes are calculated"
  (labels ((%ewl (tree)
             (if (or (equal (node-val tree) '+) (equal (node-val tree) '-) (equal (node-val tree) '*))
                 (let* ((tmp-remove (remove-if-not #'numberp (mapcar #'node-val (mapcar #'%ewl (node-children tree)))))
                       (char (remove-if #'numberp (mapcar #'%ewl (node-children tree)) :key #'node-val)))
                   (if char
                       (if tmp-remove
                           (make-node
                            :val (node-val tree)
                            :children (append (list
                                               (make-node
                                                :val (reduce (node-val tree) tmp-remove)
                                                :children nil)) char))
                           (make-node
                            :val (node-val tree)
                            :children char))
                       (make-node
                        :val (apply (node-val tree) tmp-remove)
                        :children nil)))
                 tree)))
    (let ((tmp (%ewl tree)))
      (if (node-children tmp)
          tmp
          (node-val tmp)))))

;; (eval-with-letters (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; 71
;;
;; (eval-with-letters (create-tree '(+ 1 2 (* 3 a 5) (- 10 2))))
;; #S(NODE
;;    :VALUE +
;;    :CHILDREN (#S(NODE :VALUE 11 :CHILDREN NIL)
;;               #S(NODE
;;                  :VALUE *
;;                  :CHILDREN (#S(NODE :VALUE 15 :CHILDREN NIL)
;;                             #S(NODE :VALUE A :CHILDREN NIL)))))
;;
;; (eval-with-letters (get-tree '(+ 1 2 3 (* 4 5) (- 10 a))))
;; #S(NODE
;;    :VAL +
;;    :CHILDREN (#S(NODE :VAL 26 :CHILDREN NIL)
;;               #S(NODE
;;                  :VAL -
;;                  :CHILDREN (#S(NODE :VAL 10 :CHILDREN NIL)
;;                             #S(NODE :VAL A :CHILDREN NIL)))))

(defun eval-with-letters-with-hash (tree hash)
  "change letters to values from hash-table and eval this new tree"
  (eval-with-letters (map-tree tree (lambda (key) (or (gethash key hash) key)))))

;; (defparameter *hash-table* (make-hash-table))
;; (setf (gethash 'a *hash-table*) 1)
;; (setf (gethash 'b *hash-table*) 2)
;;
;; (eval-with-letters-with-hash (get-tree '(+ 1 2 (* 3 a 5) (- 10 2))) *hash-table*)
;; 26
;;
;; (eval-with-letters-with-hash (get-tree '(+ 1 2 (* 3 a 5) (- 10 b))) *hash-table*)
;; 26
;;
;; (eval-with-letters-with-hash (get-tree '(+ 1 c (* 3 a 5) (- 10 b))) *hash-table*)
;; #S(NODE
;;   :VAL +
;;   :CHILDREN (#S(NODE :VAL 24 :CHILDREN NIL)
;;              #S(NODE :VAL C :CHILDREN NIL)))

(defun equal-trees (tree1 tree2)
  "compare 2 trees. If they are equal - T, otherwise nil"
  (when (= (length (node-children tree1)) (length (node-children tree2)))
    (and (equal (node-val tree1) (node-val tree2))
         (every #'equal-trees (node-children tree1) (node-children tree2)))))

;; (equal-trees (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))) (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; T
;;
;; (equal-trees (get-tree '(+ 1 2 (* 3 5 4) (- 10 2))) (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; NIL

(defun filterp (val)
  "use this function as a stop-predicate for filter function"
  (equal val '*))

;; (filterp '*)
;; T
;;
;; (filterp 3)
;; NIL

(defun filter (tree func stop-p)
  "if stop-p return T, stop traversal of the current sub-tree and go to the next one.
As the result return new tree without missed sub-trees."
  (if (funcall stop-p (node-val tree))
      nil
      (make-node
       :val (funcall func (node-val tree))
       :children (remove-if #'null (mapcar (lambda (tree-node) (filter tree-node func stop-p))
                                           (node-children tree))))))

;; (filter (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))) #'callback #'filterp)
;; #S(NODE
;;    :VAL +
;;    :CHILDREN (#S(NODE :VAL 0 :CHILDREN NIL)
;;               #S(NODE :VAL 1 :CHILDREN NIL)
;;               #S(NODE
;;                  :VAL -
;;                  :CHILDREN (#S(NODE :VAL 9 :CHILDREN NIL)
;;                             #S(NODE :VAL 1 :CHILDREN NIL)))))

(defun template-equal (tpl tree)
  "compare the tree with a template. Return T, if the tree corresponds the template, otherwise -- nil."
  (if (equal '? (node-val tpl))
      t
      (when (= (length (node-children tpl)) (length (node-children tree)))
        (and (equal (node-val tpl) (node-val tree))
             (every #'template-equal (node-children tpl) (node-children tree))))))

;; (template-equal (get-tree '(+ 1 2 ? (- 10 2))) (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; T
;;
;; (template-equal (get-tree '(+ 1 2 ? (- a b))) (get-tree '(+ 1 2 (* 3 4 5) (- 10 2))))
;; NIL

(defun template-equal-with-hash-result (tpl tree)
  "compare a tree with a template, and if it corresponds to the template return a hash-table
where key == value of  node form template and value -- node from tree. Otherwise -- return nil."
  (let ((hash-table (make-hash-table :test #'equal)))
    (labels ((%twhr (tpl tree)
             (cond
               ((equal '? (node-val tpl)) t)
               ((and (not (numberp (node-val tpl)))
                     (equal (char (symbol-name (node-val tpl)) 0) #\?))
                (setf (gethash (node-val tpl) hash-table) (node-val tree)))
               ((= (length (node-children tpl)) (length (node-children tree)))
                (and (equal (node-val tpl) (node-val tree))
                     (every #'%twhr (node-children tpl) (node-children tree)))))))
      (if (%twhr tpl tree)
          hash-table
          nil))))

;; (gethash '?a (template-equal-with-hash-result (get-tree '(+ 1 2 ? (- ?a 2))) (get-tree '(+ 1 2 (* 3 5 a) (- 10 2)))))
;; 10
;;
;; (gethash '?a (template-equal-with-hash-result (get-tree '(+ 1 2 ?a (- 10 2))) (get-tree '(+ 1 2 3 4 5))))
;; Error because NIL is no of type HASH-TABLE