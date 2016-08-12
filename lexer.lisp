(defun symbol-attr (sym)
  "return attribyte of symbol"
  (cond
    ((digit-char-p sym) 'number)
    ((alpha-char-p sym) 'ident)
    ((member sym '(#\+ #\- #\* #\/)) 'operator)
    ((member sym '(#\Space #\Tab)) 'whitespace)
    ((member sym '(#\: #\=)) 'assign)
    ((equal #\; sym) 'semicolon)
    ((equal sym #\)) 'r-bracket)
    ((equal sym #\() 'l-bracket)))

(defun number-p (sym)
  "predicate. Return T, if sym is number. NIL on otherwise"
  (equal (symbol-attr sym) 'number))

(defun whitespace-p (sym)
  "predicate. Return T, if sym is whitespace. NIL on otherwise"
  (equal (symbol-attr sym) 'whitespace))

(defun get-position-number (str)
  "return position of not-number in string. If it's away,
  return 1 -- length of actual number"
  (if (null (position-if-not #'number-p str))
      1
      (position-if-not #'number-p str)))

(defun get-operator-type (operator)
  "return type of math operator"
  (case operator
    (#\+ 'add)
    (#\- 'sub)
    (#\* 'mul)
    (#\/ 'div)))

(defun lexer (str)
  "Lexical analyzer takes a string-expression and returns
  a list of lists of tokens and their types"
  (labels ((lex-analys (str acc)
             (if (not (equal str ""))
                 (case (symbol-attr (char str 0))
                   (operator
                    (lex-analys (subseq str 1) (push (get-operator-type (char str 0)) acc)))
		   (assign
		    (when (and (equal (char str 0) #\:) (equal (char str 1) #\=))
		      (lex-analys (subseq str 2) (push 'assign acc))))
                   (l-bracket
                    (lex-analys (subseq str 1) (push 'l-bracket acc)))
		   (r-bracket
                    (lex-analys (subseq str 1) (push 'r-bracket acc)))
		   (semicolon
		    (lex-analys (subseq str 1) (push 'semicolon acc)))
                   (number
                    (lex-analys (subseq str (get-position-number str))
                                (push (list 'number (subseq str 0 (get-position-number str))) acc)))
		   (ident
		    (let ((st (subseq str 0 (position-if-not #'alpha-char-p str)))
			  (len (if (null (position-if-not #'whitespace-p str))
				   1
				   (position-if #'whitespace-p str))))
		      (if (equal st "const")
			  (lex-analys (subseq str len) (push 'const acc))
			  (lex-analys (subseq str len) (push 'ident acc)))))
                   (whitespace
                    (lex-analys (subseq str (position-if-not #'whitespace-p str)) acc))
                   (t
                    (lex-analys (subseq str 1) (push (list 'error-lex (char str 0)) acc))))
                 acc)))
    (reverse (lex-analys str nil))))
