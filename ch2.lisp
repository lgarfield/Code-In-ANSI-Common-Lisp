;;quote
(quote (+ 3 5))

'(+ 3 5)

(list '(+ 2 1) (+ 2 1))

()

nil

(cons 'a '(b c d))

(list 'a (cons 'b nil))

(list 'a 'b)

(car '(a b c))

(cdr '(a b c))

(third '(a b c d))

;; true or false = boolean
(listp '(a b c))

(listp 27)

(null nil)

(not nil)

;; if
(if (listp '(a b c))
    (+ 1 2)
    (+ 5 6))

(if (listp 27)
    (+ 1 2)
    (+ 3 4))

(if (listp 27)
    (+ 1 2))

;; not nil then true
(if 27 1 2)

;; function
(defun our-third (x)
  (car (cdr (cdr (x)))))

(> (+ 1 4) 3)

(defun sum-greater (x y z)
  (> (+ x y) z))

;; recursion
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

;; input and output
(format t "~A plus ~A equals ~A. ~%", 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (read))

(askem "How old are you?")

;; Variables
(let ((x 1) (y 2))
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number.")
  (let ((val (read))
        (if (numberp val)
            val
            (ask-number)))))

;; global variable
(defparamter *glob* 99)

(defconst limit (+ *glob* 1))

(boundp '*glob*)

;; assignment
(setf *glob* 98)

(let ((n 10))
  (setf n 2)
  n)

(setf x (list 'a 'b 'c))

(setf (car x) 'n)

(setf a 'b
      c 'd
      e 'f)

;; functional programming
(setf lst '(c a r a t))

(remove 'a lst)

(setf x (remove 'a x))

;; iteration
(defunc show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares (i end)
  (if (> i end)
      'done
      (progn
        (format t "~A~A~%" i (* i i))
        (show-squares (+ i i) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length (lst)
  (if (null lst)
      0
      (+ (our-length (cdr lst)) 1)))


;; Functions as Objects
(function +)

#'+

(apply #'+ (1 2 3))

(apply #' 1 2 3 '(4 5))

(funcall #'+ 1 2 3)

;; lambda
((x) (+ x 100))
(lambda (x) (+ x 100))

(lambda (x y)
  (+ x y))

((lambda (x) (+ x 100)) 1)

(funcall #'(lambda (x) (+ x 100))
         1)

;; types
(typep 27 'integer)
