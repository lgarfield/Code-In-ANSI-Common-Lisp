;; Shared Structure
(setf part (list 'b c))
(B C)

(setf whole (cons 'a part))
(A B C)

(tailp part whole)
T

(defun our-tailp (x y)
  (or (eql x y)
      (and (consp y)
           (our-tailp (x (cdr y))))))

(setf part (list 'b 'c)
      whole1 (cons 1 part)
      whole2 (cons 2 part))

(setf element (list 'a 'b)
      holds1 (list 1 element 2)
      holds2 (list element 3))

(defun our-copy-list (lst)
  (if (null lst)
      nil
      (cons (car lst) (our-copy-list (cdr lst)))))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

;; Modification
(setf whole (list 'a 'b 'c)
      tail (cdr whole))

(setf (second tail) 'e)
E

tail
(B E)

whole
(A B E)

;; Queues
(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(setf q1 (make-queue))
(NIL)

(progn (enqueue 'a q1)
       (enqueue 'b q1)
       (enqueue 'c q1))
(A B C)

q1
((A B C) C)

(dequeue q1)
A

(dequeue q1)
B

(enqueue 'd q1)
(C D)

;; Destructive Functions
(setf lst '(a r a b i a))
(A R A B I A)

(delete 'a lst)
(R B I)

lst
(A R B I)

(setf lst (delete 'a lst))

(defun nconc (x y)
  (if (consp x)
      (progn
        (setf (cdr (last x)) y)
        x)
      y))

(mapcan #'list
        '(a b c)
        '(1 2 3 4))
(A 1 B 2 C 3)

(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

(defun grandchildren (x)
  (mapcan #'(lambda (c)
              (copy-list (children c)))
          (children x)))

(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun grandchildren (x)
  (mappend #'children (children x)))

;; Binary Search Trees
(setf *bst* nil)
NIL

(dolist (x '(7 2 9 8 4 1 512))
  (setf *bst* (bst-insert! x *bst* #'<)))
NIL

(defun bst-insert! (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (progn (bsti obj bst <)
             bst)))

(defun bsti (obj bst <)
  (let ((elt (node-elt bst)))
    (if (eql obj elt)
        bst
        (if (funcall < obj elt)
            (let ((l (node-l bst)))
              (if l
                  (bsti obj l <)
                  (setf (node-l bst)
                        (make-node :elt obj))))
            (let ((r (node-r bst)))
              (if r
                  (bsti obj r <)
                  (setf (node-r bst)
                        (make-node :elt obj))))))))

(setf *bst* (bst-delete 2 *bst* #'<))
#<7>

(bst-find 2 *bst* #'<)
NIL

(defun bst-delete (obj bst <)
  (if bst (bstd obj bst nil nil <))
  bst)

(defun bstd (obj bst prev dir <)
  (let ((elt (node-elt bst)))
    (if (eql elt obj)
        (let ((rest (percolate! bst)))
          (case dir
            (:1 (setf (node-l prev) rest))
            (:r (setf (node-r prev) rest))))
        (if (funcall < obj elt)
            (if (node-l bst)
                (bstd obj (node-l bst) bst :l <))
            (if (node-r bst)
                (bstd obj (node-r bst) bst :r <))))))

(defun percolate! (bst)
  (cond ((null (node-l bst))
         (if (null (node-r bst))
             nil
             (rperc! bst)))
        ((null (node-r bst)) (lperc! bst))
        (t (if (zerop (random 2))
               (lperc! bst)
               (rperc! bst)))))

(defun lperc! (bst)
  (setf (node-elt bst) (node-elt (node-l bst)))
  (percolate! (node-l bst)))

(defun rperc! (bst)
  (setf (node-elt bst) (node-elt (node-r bst)))
  (percolate! (node-r bst)))

;; Doubly-Linked Lists
(defstruct (dl (:print-function print-dl))
  prev data next)

(defun print-dl (dl stream depth)
  (declare (ignore depth))
  (format stream "#<DL ~A>" (dl->list dl)))

(defun dl->list (lst)
  (if (dl-p lst)
      (cons (dl-data lst) (dl->list (dl-next lst)))
      lst))

(defun dl-insert (x lst)
  (let ((elt (make-dl :data x :next lst)))
    (when (dl-p lst)
      (if (dl-prev lst)
          (setf (dl-next (dl-prev lst)) elt
                (dl-prev elt) (dl-prev lst)))
      (setf (dl-prev lst) elt))
    elt))

(defun dl-list (&rest args)
  (reduce #'dl-insert args
          :from-end t :initial-value nil))

(defun dl-remove (lst)
  (if (dl-prev lst)
      (setf (dl-next (dl-prev lst)) (dl-next lst)))
  (if (dl-next lst)
      (setf (dl-prev (dl-next lst)) (dl-prev lst)))
  (dl-next lst))

(dl-list 'a 'b 'c)
#<DL (A B C)>

(dl-insert 'a (dl-insert 'b (dl-insert 'c nil)))

(setf dl (dl-list 'a 'b))
#<DL (A B) >

(setf dl (dl-insert 'c dl))
#<DL (C A B) >

(dl-insert 'r (dl-next dl))
#<DL (R A B) >

dl
#<DL (C R A B) >

;; Circular Structure
(setf x (list 'a))
(A)

(progn (setf (cdr x) x) nil)
NIL

(setf *print-circle* t)
T

x
#1= (A . #1#)

(defun circular (lst)
  (setf (cdr (last lst)) lst))

(let ((y (list 'a)))
  (setf (car y) y)
  y)
#i= (#i#)

(let ((c (cons 11)))
  (setf (car c) c
        (cdr c) c)
  c)
#1= (#1# . #1#)

(progn (defstruct elt
         (parent nil) (child nil))
       (let ((c (make-elt))
             (p (make-elt)))
         (setf (elt-parent c) p
               (elt-child p) c)
         c))
#1=#S (ELT PARENT #S (ELT PARENT NIL CHILD #1#) CHILD NIL)

;; Constant Structure
(defun arith-op (x)
  (member x '(+ - * /)))

(nconc (arith-op 'x) '(as i t were))
(* / AS IT WERE)

(arith-op 'as)
(AS IT WERE)

(defun arith-op (x)
  (member x (list '+ '- '* '/)))

(defun arith-op (x)
  (find x '(+ - * /)))
