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
