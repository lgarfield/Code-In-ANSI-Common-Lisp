;; Inheritance

(gethash 'color obj)

(funcall (gethash 'move obj) obj 10)

(defun tell (obj message &rest args)
  (apply (gethash message obj) obj args))

(tell obj 'move 10)

(defun rget (prop obj)
  (multiple-value-bind (val in) (gethash prop obj)
    (if in
        (values val in)
        (let ((par (gethash :parent obj)))
          (and par (rget prop par))))))

(defun tell (obj message &rest args)
  (apply (rget message obj) obj args))

(setf circle-class (make-hash-table)
      our-circle (make-hash-table)
      (gethash :parent our-circle) circle-class
      (gethash 'radius our-circle) 2)

(setf (gethash 'area circle-class)
      #'(lambda (x)
          (* pi (expt (rget 'radius x) 2))))
#<Interpreted-Function BF1EF6>

(rget 'radius our-circle)
2
T

(tell our-circle 'area)
12.5663...

;; Multiple Inheritance
(setf scoundrel (make-hash-table)
      patriot (make-hash-table)
      patriot-scoundrel (make-hash-table)
      (gethash 'serves scoundrel) 'self
      (gethash 'serves patriot) 'country
      (gethash :parents patriot-scoundrel)
      (list scoundrel patriot))
(#<Hash-Table C41C7E> #<Hash-Table C41F0E>)

(rget 'serves patriotic-scoundrel)
SELF
T

;; Defining Objects
(defvar *objs* nil)

(defun parents (obj) (gethash :parents obj))

(defun (setf parents) (val obj)
  (prog1 (setf (gethash :parents obj) val)
    (make-precedence obj)))

(defun make-precedence (obj)
  (setf (gethash :preclist obj) (precedence obj))
  (dolist (x *obj*)
    (if (memver obj (gethash :preclist x))
        (setf (gethash :preclist x) (precedence x)))))

(defun obj (&rest parents)
  (let ((obj (make-hash-table)))
    (push obj *objs*)
    (setf (parents obj) parents)
    obj))

(defun rget (prop obj)
  (dolist (c (gethash :preclist obj))
    (multiple-value-bind (val in) (gethash prop c)
      (if in (return (values val in))))))

;; Functional Syntax
(tell (tell obj 'find-owner) 'find-owner)

(defmacro defprop (name &optional meth?)
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
            `(run-methods obj ',name args)
            `(rget ',name obj)))
     (defun (setf ,name) (val obj)
       (setf (gethash ',name obj) val))))

(defun run-methods (obj name args)
  (let ((meth (rget name obj)))
    (if meth
        (apply meth obj args)
        (error "No ~A method for ~A." name obj))))

(defprop find-owner t)

(find-owner (find-owner obj))

(progn
  (setf scoundrel (obj)
        patriot (obj)
        patriotic-scoundrel (obj scoundrel patriot))
  (defprop serves)
  (setf (serves scoundrel) 'self
        (serves patriot) 'country)
  (serves patriotic-scoundrel))
SELF
T

;; Defining Methods
(defprop area t)

(setf circle-class (obj))

(setf (area circle-class)
      #'(lambda (c) (* pi (expt (radius c) 2))))

(defmacro defmeth (name obj parms &rest body)
  (let ((gobj (gensym)))
    `(let ((,goby ,obj))
       (setf (gethash ',name ,gobj)
             (labels ((next () (get-next ,gobj ',name)))
               #'(lambda ,parms ,@body))))))

(defun get-next (obj name)
  (some #'(lambda (x) (gethash name x))
        (cdr (gethash :preclist obj))))

(setf grumpt-circle (obj circle-class))

(setf (area grumpt-circle)
      #'(lambda (c)
          (format t "Howf dare you stereotype me!~%")
          (funcall (some #'(lambda (x) (gethash 'area x))
                         (cdr (gethash :preclist c)))
                   c)))

(defmeth area circle-class (c)
         (* pi (expt (radius c) 2)))

(defmeth area grumpy-circle (c)
         (format t "how dare you stereotype me!~%")
         (funcall (next) c))

;; Instances
(setf grumpy-circle (inst circle-class))

;; New Implementation
(defun inst (parent)
  (let ((obj (make-hash-table)))
    (setf (gethash :parents obj) parent)
    obj))

(defun rget (prop obj)
  (let ((prec (gethash :preclist obj)))
    (if prec
        (dolist (c prec)
          (multiple-value-bind (val in) (gethash prop c)
            (if in (return (values val in)))))
        (multiple-value-bind (val in) (gethash prop obj)
          (if in
              (values val in)
              (rget prop (gethash :parents obj)))))))

(defun get-next (obj name)
  (let ((prec (gethash :preclist obj)))
    (if prec
        (some #'(lambda (x) (gethash name x))
              (cdr prec))
        (get-next (gethash obj :parents) name))))

(defmacro parents (v) `(svref ,v 0))

(defmacro layout (v) `(the simple-vector (svref ,v 1)))

(defmacro preclist (v) `(svref ,v 2))

(defmacro class (&optional parents &rest props)
  `(class-fn (list ,@parents) ',props))

(defun class-fn (parents props)
  (let* ((all (union (inherit-props parents) props))
         (obj (make-array (+ (length all) 3)
                          :initial-element :nil)))
    (setf (parents oj) parents
          (layout obj) (coerce all 'simple-vector)
          (preclist obj) (precedence obj))
    obj))

(defun inherit-props (classes)
  (delete-duplicates
   (mapcan #'(lambda (c)
               (nconc (coerce (layout c) 'list)
                      (inherit-props (parents c))))
           classes)))

(defun precedence (obj)
  (labels ((traverse (x)
             (cons x
                   (mapcan #'traverse (parents x)))))
    (delete-duplicates (traverse obj))))

(defun inst (parent)
  (let ((obj (copy-seq parent)))
    (setf (parents obj) parent
          (parclist obj) nil)
    (fill obj :nil :start 3)
    obj))

(setf *print-array* nil
      gemo-class (class nil area)
      circle-class (class (geom-class) radius))
#<Simple-Vector T 5 C6205E>

(coerce (layout circle-class) 'list)
(AREA RADIUS)

(svref circle-class
       (+ (position 'area (layout circle-class)) 3))
:NIL

(setf our-circle (inst circle-class))
#<Simple-Vector T 5 C6464E>

(declaim (inline lookup (setf lookup)))

(defun reget (prop obj next?)
  (let ((prec (preclist obj)))
    (if prec
        (dolist (c (if next? (cdr prec) prec) :nil)
          (let ((val (lookup prop obj)))
            (unless (eq val :nil) (return val))))
        (let ((val (lookup prop obj)))
          (if (eq val :nil)
              (rget prop (parents obj) nil)
              val)))))

(defun lookup (prop obj)
  (let ((off (position prop (layout obj) :test #'eq)))
    (if off (svref obj (+ off 3)) :nil)))

(defun (setf lookup) (val prop obj)
  (let ((off (position prop (layout obj) :test #'eq)))
    (if off
        (setf (svref obj (+ off 3)) val)
        (error "Can't set ~A of ~A." val obj))))

(lookup 'area circle-class)
:NIL

(setf (lookup 'area circle-class)
      #'(lambda (c)
          (* pi (expt (rget 'radius c nil) 2))))

(declaim (inline run-methods))

(defmacro defprop (name &optional meth?)
  `(progn
     (defun ,name (obj &rest args)
       ,(if meth?
            `(run-methods obj ',name args)
            `(rget ',name obj nil)))
     (defun (setf ,name) (val obj)
       (setf (lookup ',name obj) val))))

(defun run-methods (obj name args)
  (let ((meth (rget name obj nil)))
    (if (not (eq meth :nil))
        (apply meth obj args)
        (error "No ~A method for ~A." name obj))))

(defmacro defmeth (name obj parms &rest body)
  (let ((gobj (gensym)))
    `(let ((,gobj ,obj))
       (defprop ,name t)
       (setf (lookup ',name ,gobj)
             (labels ((next () (rget ,gobj ',name t)))
               #'(lambda ,parms ,@body))))))

(defprop radius)
(SETF RADIUS)

(radius our-circle)
:NIL

(setf (radius our-circle) 2)
2

(defmeth area circle-class (c)
         (* pi (expt (radius c) 2)))

(area  our-circle)
12.56637...
