;; Breakloop
(/ 1 0)
Error: Division by zero.
Options: :abort, :backtrace
>> :abort
>

(/2 0)
Error: Division by zero.
Options: :abort, :backtrace, :previous
>>>

;; Traces and Backtraces
(defun treel+ (tr)
  (cond ((null tr) nil)
        ((atom tr) (1+ tr))
        (t (cons (treel+ (car tr))
                 (treel+ (cdr tr))))))

(trace treel+)
(treel+)

(trace foo)

(untrace foo)

(untrace)

;; When Noting Happens
(defun blow-stack () (1+ (blow-stack)))
BLOW-STACK

(blow-stack)
Error: Stack Overflow

(car nil)
NIL

(cdr nil)
NIL

(defun our-member (obj lst)
  (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst))))

(our-member obj nil)

(format t "for example ~A~%" 'this)

;; No Value/Unbound
(progn
  (let ((x 10))
    (format t "Here x = ~A. ~%" x))
  (format t "But now it's gone...~%")
  x)
Here x = 10.
But now it's gone...
Error: X has no value.

(defun foo (x) (+ x 1))
Error: DEFUN has no value.

;; Unexpected Nils
(defun month-length (mon)
  (case mon
    ((jan mar may jul aug dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-year) 29 28))))

(defun month-weeks (mon) (/ (month-length mon) 7.0))

(month-weeks 'oct)
Error: NIL is not a valid argument to /.

;; Renaming
(defun depth (x)
  (if (atom x)
      1
      (1+ (apply #'max (mapcar #'depth x)))))

(depth '((a)))
3

(defun nesting-depth (x)
  (if (atom x)
      0
      (1+ (apply #'max (mapcar #'depth x)))))
(nesting-depth '((a)))
3

;; Keywords as Optional Parameters
(read-from-string &optional eof-error eof-value
                  &key start end preserve-whitespace)

(read-from-string "abcd" :start 2)
ABCD
4

(read-from-string "abcd" nil nil :start 2)
CD
4

;; Misdeclarations
(defun df* (a b)
  (declare (double-float a b))
  (* a b))

(df* 2 3)
Error: Interrupt.

;; Warnings
(map-int #'(lambda (x)
             (declare (ignore x))
             (random 100))
         10)
