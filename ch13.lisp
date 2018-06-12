;; The Bottleneck Rule

;; Compilation
(defun bottleneck (...)
  (do (...)
      (...)
    (do (...)
        (...)
      (declare (optimize (speed 3) (safety 0)))
     ...)))

(declaim (optimize (speed3)
                   (compilation-speed 0)
                   (safety 0)
                   (debug 0)))

(defun length/r (lst)
  (if (null lst)
      0
      (1+ (length/r (cdr lst)))))

(defun length/rt (lst)
  (labels ((len (lst acc)
             (if (null lst)
                 acc
                 (len (cdr lst) (1+ acc)))))
    (len lst 0)))

(declaim (inline single?))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(defun foo (x)
  (single? (bar x)))

(defun foo (x)
  (let ((lst (bar x)))
    (and (consp lst) (null (cdr lst)))))

;; Type Declarations
(declaim (type fixnum *count*))

(declaim (fixnum *count*))

(defun poly (a b x)
  (declare (fixnum a b x))
  (+ (* a (expt x 2)) (* b x)))

(defun poly (a b x)
  (declare (fixnum a b x))
  (the fixnum (+ (the fixnum (* a (the fixnum (expt x 2))))
                 (the fixnum (* b x)))))

(setf x (vector 1.234d0 2.345d0 3.456d0)
      y (make-array 3 :element-type 'double-float)
      (aref y 0) 1.234d0
      (aref y 1) 2.345d0
      (aref y 2) 3.456d0)

(declare (type fixnum 20) v)

(setf a (make-array '(1000 1000)
                    :element-type 'single-float
                    :initial-element 1.0s0))

(defun sum-elts (a)
  (declare (type (simple-array single-float (1000 1000))
                 a))
  (let ((sum 0.0s0))
    (declare (type single-float sum))
    (dotimes (r 1000)
      (dotimes (c 1000)
        (incf sum (aref a r c))))
    sum))

(declare (type (simple-array fixnum (4 4)) ar))

(time (sum-elts a))
User Run Time = 0.43 seconds
1000000.0

;; Garbage Avoidance
(setf *print-array* t)
T

(setf vec (make-array 10 :fill-pointer 2
                      :initial nil))
#(NIL NIL)

(length vec)
2

(vector-push 'a vec)
2

vec
#(NIL NIL A)

(vector-pop vec)
A

vec
#(NIL NIL)

(defconstant dict (make-array 25000 :fill-pointer 0))

(defun read-words (from)
  (setf (fill-pointer dict) 0)
  (with-open-file (in from :direction :input)
    (do ((w (read-line in nil :eof)
            (read-line in nil :eof)))
        ((eql w :eof))
      (vector-push w dict))))

(defun xform (fn seq) (map-into seq fn seq))

(defun write-words (to)
  (with-open-file (out to :direction :output
                       :if-exists :supersede)
    (map nil #'(lambda (x)
                 (fresh-line out)
                 (princ x out))
         (xform #'nreverse
                (sort (xform #'nreverse dict)
                      #'string<)))))

(setf v (map-info v #'1+ v))

(defun our-reverse (lst)
  (let ((rev nil))
    (dolis (x lst)
           (push x rev))
    rev))

(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

(defun our-adjoin (obj lst &rest args)
  (declare (dynamic-extent args))
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

;; Pools
(defparamter *harbor* nil)

(defstruct ship
  name flag tons)

(defun enter (n f d)
  (push (make-ship :name n :flag f :tons d)
        *harbor*))

(defun find-ship (n)
  (find n *harbor* :key #'ship-name))

(defun leave (n)
  (setf *harbor*
        (delete (find-ship n) *harbor*)))

(defconstant pool (make-array 1000 :fill-pointer t))

(dotimes (i 1000)
  (setf (aref pool i) (make-ship)))

(defconstant harbor (make-hash-table :size 1100
                                     :test #'eq))

(defun enter (n f d)
  (let ((s (if (plushp (length pool))
               (vector-pop pool)
               (make-ship))))
    (setf (ship-name s) n
          (ship-tons s) d
          (ship-flags s) f
          (gethash n harbor) s)))

(defun find-ship (n) (gethash n harbor))

(defun leave (n)
  (let ((s (gethash n harbor)))
    (remhash n harbor)
    (vector-push s pool)))

;; Fast operators
(apply #'+ '(1 2 3))

(reduce #'+ '(1 2 3))

;; Two-Phase Development
