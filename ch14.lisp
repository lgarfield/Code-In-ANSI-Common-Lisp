;; Type Specifiers
(or vector (and list (not (staisfies circular?))))

(integer 1 100)

(simple-array fixnum (* *))

(simple-array fixnum *)

(simple-array fixnum)

(deftype proseq ()
  '(or vector (and list (not (satisfies circular?)))))

(typep #(1 2) 'proseq)
T

(deftype multiple-of (n)
  `(and integer (satisfies (lambda (x)
                             (zerop (mod x ,n))))))

(type 12 '(multiple-of 4))
T

;; Binary Streams
(defun copy-file (from to)
  (with-open-file (in from :direction :input
                      :element-type 'unsigned-byte)
    (with-open-file (out to :direction :output
                         :element-type 'unsigned-byte)
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (declare (fixnum i))
        (write-byte i out)))))

(unsigned-byte 7)

;; Read-Macros
(set-macro-charactor #\'
                     #'(lambda (stream char)
                         (list (quote quote) (read stream t nil t))))

(read &optional stream eof-error eof-value recursive)

(set-dispatch-macro-charactor #\# #\?
                              #'(lambda (stream char1 char2)
                                  (list 'qutoe
                                        (let ((lst nil))
                                          (dotimes (i (+ (read stream t nil t) 1))
                                            (push i lst))
                                          (nreverse lst)))))

#?7
(1 2 3 4 5 6 7)

(set-macro-character #} (get-macro-character #\)))

(set-dispatch-macro-character #\# #\{
                              #'(lambda (stream char1 char2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #} stream t)))
                                    (do ((i (car pair) (+ i 1)))
                                        ((> i (cadr pair))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))

#{2 7}
(2 3 4 5 6 7)

;; Packages
(package-name *package*)
"COMMON-LISP-USER"

(find-package "COMMON-LISP-USER")
#<Package "COMMON-LISP-USER" 4CD15E>

(symbol-package 'sym)
#<Package "COMMON-LISP-USER" 4CD15E>

(setf sym 99)
99

(setf *package* (make-package 'mine
                              :use '(common-lisp)))
#<Package "MINE" 63390E>

(in-package common-lisp-user)
#<Package "COMMON-LISP-USER" 4CD15E>

(export 'bar)
T

(setf bar 5)
5

(in-package mine)
#<Package "MINE" 63390E>

common-lisp-user:bar
5

(import 'common-lisp-user:bar)
T

bar
5

(import 'common-lisp-user::sym)
Error: SYM is already present in MINE.

(use-package 'common-lisp-user)
T

;; The Loop Facility
(loop for x from 0 to 9
   do (princ x))
0123456789
NIL

(loop for x = 8 then (/ x 2)
   until (< x 1)
   do (princ x))
8421
NIL

(loop for x from 1 to 4
   and y from 1 to 4
   do (princ (list x y)))
(1 1) (2 2) (3 3) (4 4)
NIL

(loop for x in '(1 2 3 4)
   collect (1+ x))
(2 3 4 5)

(loop x from 1 to 5
   collect (random 10))
(3 8 5 1 0)

(defun even/odd (ns)
  (loop for n in ns
     if (evenp n)
     collect n into evens
     else collect n into odds
     finally (return (values evens odds))))

(defun sum (n)
  (loop for x from 1 to n
     sum x))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
        (values wins max))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (loop with wins = (car lst)
         with max = (funcall fn wins)
         for obj in (cdr lst)
         for score = (funcall fn obj)
         when (> score max)
           (do (setf wins obj
                     max score)
               finally (return (values wins max))))))

(defun num-year (n)
  (if (< n 0)
      (loop for y downfrom (- yzero 1)
         until (<= d n)
         sum (- (year-days y)) into d
         finally (return (values (+ y 1) (- n d))))
      (loop with prev = 0
         for y from yzero
         until (> d n)
         do (setf prev d)
         sum (year-days y) into d
         finally (return (values (- y 1)
                                 (- n prev))))))

(loop for y = 0 then z
   for x from 1 to 5
   sum 1 into z
   finally (return y z))

(loop for x from 1 to 5
   for y = 0 then z
   sum 1 into z
   finally (return y z))

;; Conditions
(error "Your report uses ~A as a verb." 'status)
Error: Your report uses STATUS as a verb
Options: :abort, :backtrace

(ecase 1 (2 3) (4 5))
Error: No applicable clause
Options: :abort, :backtract

(let ((x '(a b c)))
  (check-type (car x) integer "an integer")
  x)
Error: The value of (CAR X), A, should be an integer.
Options: :abort, :backtrace, :continue

:continue
99
(99 B C)

(let ((sandwich '(ham on rye)))
  (assert (eql (car sandwich) 'chicken)
          ((car sandwich))
          "I wanted a ~A sandwich." 'chicken)
  sandwich)
Error: I wanted a CHICKEN sandwich.
Options: :abort, :backtract, :continue

:continue
'chicken
(CHICKEN ON RYE)

(defun user-input (prompt)
  (format t promot)
  (let ((str (read-line)))
    (or (ignore-errors (read-from-string str))
        nil)))

(use-input "Please type an expression")
Please type an expression> #121#@&$^&Q
NIL
