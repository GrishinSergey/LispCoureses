;; Hometask "Macros"


(defun and-f (args)
  (if args
      `(let* ((it ,(car args)))
     (when it
       ,(and-f (cdr args))))
      'it))

(defmacro a-and (&rest args)
  (and-f args))

;;-----------------------------------------

(defun cond-f (args)
  (if args
      `(let ((it ,(car (car args))))
     (if it
         (if ,(eq nil (rest (car args)))
         it
         ,(car (cdr (car args))))
         ,(cond-f (rest args))))))

(defmacro acond (&body body)
  (cond-f body))

;;-----------------------------------------

(defmacro cut (&body body)
  (let (args)
    (labels ((%cut-f (lst)
               (mapcar (lambda (sym)
             (if (listp sym)
                 (%cut-f sym)
                 (if (equal sym '_)
                 (let* ((it (gensym)))
                   (push it args) it)
                 sym)))
               lst)))
      (let ((new-body (%cut-f body)))
        `(lambda ,(reverse args) ,new-body)))))

;;-----------------------------------------

(defun my-let-f (binds body)
  (when binds
    `((lambda (,(if (listp (car binds))
            (car (car binds))
            (car binds)))
    ,@(if (rest binds)
          (list (my-let-f (rest binds) body))
          body))
      ,(when (listp (car binds))
     (car (cdr (car binds)))))))

(defmacro my-let* (binds &body body)
  (my-let-f binds body))

;;-----------------------------------------

(defun mv-let-f (binds body)
  (if binds
      `(multiple-value-bind ,(car (car binds)) ,(car (cdr (car binds)))
     ,@(if (rest binds)
           (list (mv-let-f (cdr binds) body))
           binds))))

(defmacro mv-let* (binds &body body)
  (mv-let-f binds body))
