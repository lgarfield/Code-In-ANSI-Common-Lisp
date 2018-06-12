;; Object-Oriented Programming
(defstruct rectangle
  height width)

(defstruct circle
  radius)

(defun area (x)
  (cond ((rectangle-p x)
         (* (rectangle-height x) (rectangle-width x)))
        ((circle-p x)
         (* pi (expt (circle-radius x) 2)))))

(let ((r (make-rectangle)))
  (setf (rectangle-height r) 2
        (rectangle-width r) 3)
  (area r))
6

(defclass rectangle ()
  (height width))

(defclass circle ()
  (radius))

(defmethod area ((x rectangle))
  (* (slot-value x 'height) (slot-value x 'width)))

(defmethod area ((x circle))
  (* pi (expt (slot-value x 'radius) 2)))

(let ((r (make-instance 'rectangle)))
  (setf (slot-value r 'height) 2
        (slot-value r 'width) 3)
  (area r))
6

(defclass colored ()
  (color))

(defclass colored-circle (circle colored)
  ())

;; Class and Instances
(defclass circle ()
  (radius center))

(setf c (make-instance 'circle))
#<CIRCLE #XC27496>

(setf (slot-value c 'radius) 1)
1

;; Slot Properties
(defclass circle ()
  ((radius :accessor circle-radius)
   (center :accessor circle-center)))

(setf c (make-instance 'circle))
#<CIRCLE #XC5C726>

(setf (circle-radius c) 1)
1

(circle-radius c)
1

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 1)
   (center :accessor circle-center
           :initarg :center
           :initform (cons 0 0))))

(setf c (make-instance 'circle :radius 3))
#<CIRCLE #XC2DE0E>

(circle-radius c)
3

(circle-center c)
(0 . 0)

(defclass tabloid ()
  ((top-story :accessor tabloid-story
              :allocation :class)))

(setf daily-blab (make-instance 'tabloid)
      unsolicited-mail (make-instance 'tabloid))
#<TABLOID #X302000EFE5BD>

(setf (tabloid-story daily-blab) 'adultery-of-senator)
ADULTERY-OF-SENATOR

(tabloid-story unsolicited-mail)
ADULTERY-OF-SENATOR

;; Superclassed
(defclass graphic ()
  ((color :accessor graphic-color :initarg :color)
   (visible :accessor graphic-visible :initarg :visible
            :initform t)))

(defclass screen-circle (circle graphic) ())

(graphic-color (make-instance 'screen-circle
                              :color 'red :radius 3))
RED

(defclass screen-circle (circle graphic)
  ((color :initform 'purple)))

(graphic-color (make-instance 'screen-circle))
PURPLE

;; Precedence
(defclass sculpture () (height width depth))

(defclass statue (sculpture) (subject))

(defclass metalwork () (meta-type))

(defclass casting (metalwork) ())

(defclass cast-statue (statue casting) ())

;; Generic Functions
(defmethod combine (x y)
  (list x y))

(combine 'a 'b)
(A B)

(defclass stuff () ((name :accessor name :initarg :name)))
(defclass ice-cream (stuff) ())
(defclass topping (stuff) ())

(defmethod combine ((ic ice-cream) (top topping))
  (format nil "~A ice-cream with ~A topping."
          (name ic)
          (name top)))

(combine (make-instance 'ice-cream :name 'fig)
         (make-instance 'topping :name 'treacle))
"FIG ice-cream with TREACLE topping"

(combine 23 'skiddoo)
(23 SKIDDOO)

(defmethod combine ((ic ice-cream) x)
  (format nil "~A ice-cream with ~A."
          (name ic)
          x))

(combine (make-instance 'ice-cream :name 'grape)
         (make-instance 'topping :name 'marshmallow))
"GRAPE ice-cream with MARSHMALLOW topping"

(combine (make-instance 'ice-cream :name 'clam)
         'reluctance)
"CLAM ice-cream with RELUCTANCE"

(defmethod combine ((x number) (y number))
  (+ x y))

(defmethod combine ((x (eql 'powder)) (y (eql 'spark)))
  'boom)

(x) (a)
(x &optional y) (a &optional b)
(x y &rest z) (a b &key c)
(x y &key z) (a b &key c d)

(x) (a b)
(x &optional y) (a &optional b c)
(x &optional y) (a &rest b)
(x &key x y) (a)

(defmethod combine ((x (eql 'powder)) (y (eql 'spark)))
  'kaboom)

;; Auxiliary Methods
(defclass speaker () ())

(defmethod speak ((s speaker) string)
  (format t "~A" string))

(speak (make-instance 'speaker)
       "I'm hungry.")
I'm hungry.
NIL

(defclass intellectual (speaker) ())

(defmethod speak :before ((i intellectual) string)
  (princ "Perhaps "))

(defmethod speak :after ((i intellectual) string)
  (princ " insome sense"))

(speak (make-instance 'intellectual)
       "I'm hungry")
Perhaps I'm hungry in some sense.
NIL

(defmethod speak :before ((s speaker) string)
  (princ "I think "))

(defclass courtier (speaker) ())

(defmethod speak :around ((c courtier) string)
  (format t "Don't the King believe that ~A?" string)
  (if (eql (read) 'yeas)
      (if (next-method-p) (call-next-method))
      (format t "Indeed, it is a preposterous idea. ~%"))
  'bow)

(speak (make-instance 'courtier) "kings will last")
Does the King believe that kings will last? yes
I think kings will last
BOW

(speak (make-instance 'courtier) "kings will last")
Does the King believe that kings will last? no
Indeed, it is a preposterous idea.
BOW

;; Method Combination
(defun price (&rest args)
  (+ (apply (most specific primary method) args)
     .
     .
     .
     (apply (least specific primary method) args)))

(defgeneric price (x)
  (:method-combination +))

(defclass jacket () ())
(defclass trousers () ())
(defclass suit (jacket trousers) ())

(defmethod price + ((jk jacket)) 350)
(defmethod price + ((tr trousers)) 200)

(price (make-instance 'suit))
550


+ and append list max min nconc or progn

;; Encapsulation
(defpackage "CTR"
  (:use "COMMON-LISP")
  (:export "COUNTER" "INCREMENT" "CLEAR"))

(in-package ctr)

(defclass counter () ((state :initform 0)))

(defmethod increment ((c counter))
  (incf (slot-value c 'state)))

(defmethod clear ((c counter))
  (setf (slot-value c 'state) 0))

(unintern 'state)

;; Two Models
tell obj area

(move obj 10)
