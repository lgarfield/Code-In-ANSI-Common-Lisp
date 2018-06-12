;; Eval
(eval '(+ 1 2 3))
6

(eval '(format t "Hello"))
Hello
NIL

(defun our-toplevel ()
  (do ()
      (nil)
    (format t "~%> ")
    (print (eval (read)))))

(defun eval (expr env)
  (cond ...
        ((eql (car expr) 'quote) (cdr expr))
        ...
        (t (apply (symbol-function (car expr))
                  (mapcar #'(lambda (x)
                              (eval x env))
                          (cdr expr))))))

(coerce '(lambda (x) x) 'function)
#<Interpreted-Function BF9D96>

(compile nil '(lambda (x) (+ x 2)))
#<Compiled-Function BF55BE>
NIL
NIL

;; Macros
(defmacro nil! (x)
  (list 'setf x nil))

(setf x nil)

(nil! x)
NIL

x
NIL

(macroexpand-1 '(nil! x))
(SETF X NIL)
T

(lambda (expr)
  (apply #'(lambda (x) (list 'setf x nil))
         (cdr expr)))

;; Backquote
`(a b c)
(A B C)

(setf a 1 b 2)
2
'(a is ,a and b is ,b)
(A IS 1 AND B IS 2)

(defmacro nil! (x)
  `(setf ,x nil))

(setf lst '(a b c))
(A B C)

`(lst is ,lst)
(LST IS (A B C))

`(its elements are ,@lst)
(ITS ELEMENTS ARE A B C)

(let ((x 0))
  (while (< x 10)
    (princ x)
    (incf x)))
0123456789
NIL

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;; Quicksort
(defun quicksort (vec l r)
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2))))
    (while (<= i j)
      (while (< (svref vec i) p) (incf i))
      (while (> (svref vec j) p) (decf j))
      (when (<= i j)
        (rotatef (svref vec i) (svref vec j))
        (incf i)
        (decf j)))
    (if (>= (- j l) 1) (quicksort vec l j))
    (if (<= (- r i) 1) (quicksort vec i r)))
  vec)

;; Macro Design
(ntimes 10
        (princ "."))
..........
NIL

(defmacro ntimes (n &rest body)
  `(do ((x 0 (1+ x)))
       ((>= x ,n))
     ,@body))

(let ((x 10))
  (ntimes 5
          (setf x (+ x 1)))
  x)
10

(let ((x 10))
  (do ((x 0 (1+ x)))
      ((>= x 5))
    (setf x (+ x 1)))
  x)

(let ((v 10))
  (ntimes (setf v (1- v))
          (princ ".")))
.....
NIL

(let ((v 10))
  (do ((#:g1 0 (+ #:g1 1)))
      ((>= #:g1 (setf v (- v 1))))
    (princ ".")))

(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

(pprint (macroexpand-1 '(cond (a b)
                         (c d e)
                         (t f ))))
(IF A
    B
    (IF C
        (PROGN D E)
        F))

;; Generalized Reference
(defmacro cah (lst) `(car ,lst))

(let ((x (list 'a 'b 'c)))
  (setf (cah x) 44)
  x)
(44 B C)

(defmacro incf (x &optional (y 1)) ;; wrong
  `(setf ,x (+ ,x ,y)))

(setf (car (push 1 lst)) (1+ (car (push 1 lst))))

(incf (car (push 1 lst)))

(define-modify-macro our-incf (&optional (y 1)) +)

(define-modify-macro append1f (val)
  (lambda (lst val) (append lst (list val))))

(let ((lst '(a b c)))
  (append1f lst 'd)
  lst)
(A B C D)

;; Macro Utilities
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop, ,stop))
         ((> ,var, gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@ (mapcar #'(lambda (c) `(eql ,insym ,c))
                      choices)))))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@ (let ((key -1))
          (mapcar #'(lambda (expr)
                      `(,(incf key) ,expr))
                  exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(for x 1 8
     (princ x))
12345678
NIL

(do ((x 1 (+ x 1)))
    ((> x 8))
  (princ x))

(do ((x 1 (1+ x))
     (#:g1 8))
    ((> x #:g1))
  (princ x))

(in (car expr) '+ '- '*)

(let ((op (car expr)))
  (or (eql op '+)
      (eql op '-)
      (eql op '*)))

(random-choice (turn-left) (turn-right))

(case (random 2)
  (0 (turn-left))
  (1 (turn-right)))

(let ((x (gensym)) (y (gensym)) (z (gensym)))
  ...)

(with-gensyms (x y z)
  ...)

(avg 2 4 8)
14/3

(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

(let ((val (calculate-something)))
  (if val
      (1+ val)
      0))

(aif (calculate-something)
     (1+ it)
     0)
