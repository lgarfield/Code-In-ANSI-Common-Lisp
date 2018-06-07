;; Global Functions
(fboundp '+)
T

(symbol-function '+)
#<Compiled-function + 17BA4E>

(setf (symbol-function 'add2)
      #'(lambda (x) (+ x 2)))

(add2 1)
3

(defun add2 (x)
  (+ x 2))

(defun primo (lst) (car lst))

(defun (setf primo) (val lst)
  (setf (car lst) val))

(let ((x (list 'a 'b 'c)))
  (setf (primo x) 480)
  x)
(480 b c)

(defun foo (x)
  "Implements an enhanced paradigm of diversity"
  x)

(documentation 'foo 'function)
"Implements an enhanced paradigm of diversity"

;; Local Functions
(name paramters . body)

(labels ((add10 (x) (+ x 10))
         (consa (x) (cons 'a x)))
  (consa (add10 3)))
(A . 13)

(labels ((len (lst)
           (if (null lst)
               0
               (+ (len (cdr lst)) 1))))
  (len '(a b c)))
3

(do ((x a (b x))
     (y c (d y)))
    ((test x y) (z x y))
  (f x y))

(labels ((rec (x y)
           (cond ((test x y)
                  (z x y))
                 (t
                  (f x y)
                  (rec (b x) (d y))))))
  (rec a c))

;; Parameter Lists
(defun our-funcall (fn &rest args)
  (apply fn args))

(defun philosoph (thing &optional property)
  (list thing 'is property))

(philosoph 'death)
(DEATH IS NIL)

(defun keylist (a &key x y z)
  (list a x y z))
KEYLIST

(keylist 1 :y 2)
(1 NIL 2 NIL)

(keylist 1 :y 3 :x 2)
(1 2 3 NIL)

(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

(destructuring-bind ((&key w x) &rest y) '((:w 3) a)
  (list w x y))
(3 NIL (A))

;; Utilities
