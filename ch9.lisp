;; Types
(list (ratiop 2/2) (complexp #c (1 0)))
(NIL NIL)

;; Conversion and Extraction
(mapcar #'float '(1 2/3 .5))
(1.0 0.66666667 0.5)

(truncate 1.3)
1
0.299999995

(defun palindrome? (x)
  (let ((mid (/ (length x) 2)))
    (equal (subseq x 0 (floor mid))
           (reverse (subseq x (ceiling mid))))))

(floor 1.5)
1
0.5

(defun our-truncate (n)
  (if (> n 0)
      (floor (n))
      (ceiling n)))

(mapcar #'round '(-2.5 -1.5 1.5 2.5))
(-2 -2 2 2)

(mapcar #'signum '(-2 -0.0 0.0 0 .5 3))
(-1 -0.0 0.0 0 1.0 1)

(* (abs x) (signum x))
x

;; Comparison
(= 1 1.0)
T

(eql 1 1.0)
NIL

(<= w x y z)
(and (<= w x) (<= x y) (<= y z))

(/= w x y z)
(and (/= w x) (/= w y) (/= w z)
     (/= x y) (/= x z) (/= y z))

(list (minusp -0.0) (zerop -0.0))
(NIL T)

(list (max 1 2 3 4 5) (min 1 2 3 4 5))
(5 1)

;; Arithematic
(- x y z)

(- (- x y) z)

(/ 3)
1/3

(/ x y z)
(/ (/ x y) z)

(/ 365 12)
365/12

(float 365/12)
30.416666

;; Exponentiation
(expt x n)
(expt 2 5)
32

(log x n)
(log 32 2)
5.0

(exp 2)
7.389056

(log 7.389056)
2

(expt 27 1/3)
3.0

(sqrt 4)
2.0

;; Trigometric Functions
(let ((x (/ pi 4)))
  (list (sin x) (cos x) (tan x)))
(0.0707***d0 0.707***d0 1.0d0)

;; Representations
(values most-positive-fixnum most-negative-fixnum)
536870911
-536870912

(typep 1 'fixnum)
T

(type (1+ most-positive-fixnum) 'bignum)
T

(* most-positive-long-float 10)
Error: floating-point-overflow

;; Ray-Tracing
(defun sq (x) (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun minroot (a b c)
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))))
        (unless (minusp disc)
          (let ((discrt (sqrt disc)))
            (min (/ (+ (- b) discrt) (* 2 a))
                 (/ (- (- b) discrt) (* 2 a))))))))

(multiple-value-call #'mag (unit-vector 23 12 47))
1.0

(defstruct surface color)

(defparamter *world* nil)
(defconstant eye (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
          ((< (-50 y) inc))
        (do ((x -50 (+ x inc)))
            ((< (- 50 x) inc))
          (print (color-at x y) p))))))

(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
                   (- y (y eye))
                   (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
        (* (lambert s int xr yr zr) (surface-color s))
        0)))

(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
        (when h
          (let ((d (distance h pt)))
            (when (or (null dist) (< d dist))
              (setf surface s hit h dist d))))))
    (value surface hit)))

(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

(defstruct (sphere (:include surface))
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
            :radius r
            :center (make-pooint :x x :y y :z z)
            :color c)))
    (push s *world*)
    s))

(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
           s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-point :x (+ (x pt) (* n xr))
                    :y (+ (y pt) (* n yr))
                    :z (+ (z pt) (* n zr))))))

(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
           s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))

(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 - 1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
        ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))
