;; Blocks
(progn
  (format t "a")
  (format t "b")
  (+ 11 12))
ab
23

(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this."))
Here we go.
IDEA

(block nil
  (return 27))
27

(dolist (x '(a b c d e))
  (format t "~A" x)
  (if (eql x 'c)
      (return 'done)))
A B C
DONE

(defun foo ()
  (return-form foo 27))

(defun read-integer (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
        (if i
            (setf accum (+ (* accum 10) i))
            (return-from read-integer nil))))
    accum))

(tagbody
   (setf x 0)
 top
   (setf x (+ x 1))
   (format t "~A" x)
   (if (< x 10) (go top)))
1 2 3 4 5 6 7 8 9 10
NIL

;; Context
(let ((x 7)
      (y 2))
  (format t "Number")
  (+ x y))
Number
9

((lambda (x) (+ x 1)) 3)
4

((lambda (x y)
   (format t "Numbewr")
   (+ x y))
 7
 2)

(let ((x 2)
      (y (+ x 1)))
  (+ x y))

((lambda (x y) (+ x y)) 2
 (+ x 1))

(let* ((x 1)
       (y (+ x 1)))
  (+ x y))
3

(let (x y)
  (list x y))
(NIL NIL)

(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))
(A B C (D E))

;; Conditionals
(when (oddp that)
  (format t "Hmm, that's odd.")
  (+ that 1))

(if (oddp that)
    (progn
      (format t "Hmm, that's odd.")
      (+ that 1)))

(defun our-member (obj lst)
  (if (atom lst)
      nil
      (if (eql (car lst) obj)
          lst
          (our-member obj (cdr lst)))))

(defun our-member (obj lst)
  (cond ((atom lst) nil)
        ((eql (car lst) obj) lst)
        (t (our-member obj (cdr lst)))))

(cond (99))
99

(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-year) 29 28))
    (otherwise "unknown month")))

(case 99 (99))
NIL

;; Iteration
;; do
(variable initial update)

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(let ((x 'a))
  (do ((x 1 (+ x 1))
       (y x x))
      ((> x 5))
    (format t "(~A~A) " x y)))
(1 A) (2 1) (3 2) (4 3) (5 4)
NIL

(do* ((x 1 (+ x 1))
      (y x x))
     ((> x 5))
  (format t "(~A ~A) " x y))
(1 1) (2 2) (3 3) (4 4) (5 5)
NIL

(dolist (x '(a b c d) 'done)
  (format t "~A " x))
A B C D
DONE

(dotimes (x 5 x)
  (format t "~A " x))
0 1 2 3 4
5

(defun factorial (n)
  (do ((j n (- j 1))
       (f 1 (* j f)))
      ((= j 0) f)))

(mapc #'(lambda (x y)
          (format t "~A ~A " x y))
      '(hip flip slip)
      '(hop flop slop))
HIP HOP FLIP FLOP SLIP SLOP
(HIP FLIP SLIP)

;; Multiple Values
(values 'a nil (+ 2 4))
A
NIL
6

((lambda () (lambda () (values 1 2))))
1
2

(let ((x (values 1 2)))
  x)
1

(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))
(1 2 3)

(multiple-value-bind (x y z) (values 1 2)
  (list x y z))
(1 2 NIL)

(multiple-value-bind (s m h) (get-decoded-time)
  (format t "~A:~A:~A" h m s))
"4:32:13"

(multiple-value-call #'+ (values 1 2 3))
6

(multiple-value-list (values 'a 'b 'c))
(A B C)

;; Aborts
(defun super ()
  (catch 'abort
    (sub)
    (format t "We'll never see this.")))

(defun sub ()
  (throw 'abort 99))

(super)
99

(progn
  (error "Oops!")
  (format t "After the error."))
Error: Oops!
Options: :abort, :backtrace
>>

(setf x 1)
1

(catch 'abort
  (unwind-protect
       (throw 'abort 99)
    (setf x 2)))
99

x
2

;; Date Arithmetic
(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))
(31 28 31 30 31 30 31 31 30 31 30 31)

(apply #'+ mon)
365

(setf nom (reverse mon))
(31 30 31 30 31 31 30 31 30 31 28 31)

(setf sums (maplist #'(lambda (x)
                        (apply #'+ x))
                    nom))
(365 334 304 273 243 212 181 151 120 90 59 31)

(defconstant month
  #(0 31 59 90 120 151 191 212 243 273 304 334 365))

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
        (dotimes (i (- y yzero) d)
          (incf d (year-days (+ yzero i))))
        (dotimes (i (- yzero y) (- d))
          (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y) 366 365))

(mod 23 5)
3

(mode 25 5)
0

(mapcar #'leap? '(1904 1900 1600))
(T NIL T)

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
            (d (- (year-days y)) (- d (year-days y))))
           ((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
            (prev 0 d)
            (d (year-days y) (+ d (year-days y))))
           ((> d n) (values y (- n prev))))))

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
            ((> n 59) (nmon (- n 1)))
            (t (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (svref month (- m 1)))))))

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))
