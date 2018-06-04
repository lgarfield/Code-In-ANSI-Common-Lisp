;; Conses
(setf x (cons 'a nil))

(setf y (list 'a 'b 'c))

(setf z (list 'a (list 'b 'c) 'd))

(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x)))

;; Equality
(eql (cons 'a nil) (cons 'a nil))

(setf x (cons 'a nil))

(eql x x)

(equal x (cons 'a nil))

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

;; Why Lisp Has no Pointers
(setf x '(a b c))

(setf y x)

;; Building Lists
(setf x '(a b c)
      y (copy-list x))

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

(append '(a b) '(c d) 'e)

;; Compression
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

((defun compr (elt n lst)
   (if (null lst)
       (list (n-elts elt n))
       (let ((next (car lst)))
         (if (eql next elt)
             (compr elt (+ n 1) (cdr lst))
             (cons (n-elts elt n)
                   (compr next 1 (cdr lst))))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp (elt)
                   (append (apply #'list-of elt)
                           rest)
                   (cons elt rest))))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(load "ch1.lisp")

;; Access
(nth 0 '(a b c))
A

(nthcdr 2 '(a b c))
(C)

(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) lst)))

(last '(a b c))
(C)

;; Mapping Functions
(mapcar #'(lambda (x) (+ x 10))
        '(1 2 3))

(mapcar #'list
        '(a b c)
        '(1 2 3 4))

(maplist #'(lambda (x) x)
         '(a b c))


;; Trees
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))

;; Understanding Recursion
(defun len (lst)
  (if (null lst)
      0
      (+ (len (cdr lst)) 1)))

(defun our-member (obj lst)
  (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst))))

(+ 1 2)
3

;; Sets
(member 'b '(a b c))

(member '(a) '((a) (z)) :test #'equal)
((A) (Z))

(member 'a '((a b) (c d)) :key #'car)
(A B) (C D)

(member 2 '((1) (2)) :key #'car :test #'equal)
((2))
(member 2 '((1) (2)) :test #'equal :key #'car)
((2))

(member-if #'oddp '(2 3 4))
(3 4)

(defun our-member-if (fn lst)
  (and (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

(adjoin 'b '(a b c))
(A B C)

(adjoin 'z '(a b c))
(Z A B C)

(union '(a b c) '(c b s))
(A C B S)

(intersection '(a b c) '(b b c))
(B C)

(set-difference '(a b c d e) '(b e))
(A C D)

;; Sequences
(length '(a b c))
3

(subseq '(a b c d) 1 2)
(B)

(subseq '(a b c d) 1)
(B C D)

(reverse '(a b c))
(C B A)

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))

(mirror? '(a b b a))
T

(sort '(8 1 3 4 5) #'>)
(8 5 4 3 1)

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))

(every #'oddp '(1 3 5))
T

(some #'evenp '(1 2 3))
T

(every #'> '(1 3 5) '(0 2 4))
T

;; Stacks
(push obj lst)

(set lst (cons obj lst))

(pop x)

(let ((x (car lst)))
  (setf lst (cdr lst))
  x)

(defun our-reverse (lst)
  (let ((acc nil))
    (dolist (elt lst)
      (push elt acc))
    acc))

(let ((x '(a b)))
  (pushnew ('c x))
  (pushnew ('a x))
  x)

;; Dotted lists
(defun proper-list? (x)
  (or (null x)
      (and (consp x)
           (proper-list? (cdr x)))))

(setf pair (cons 'a 'b))
(A . B)

'(a . (b . (c . nil)))
(A B C)

(cons 'a (cons 'b (cons 'c 'd)))
(A B C . D)

;; Assoc-lists
(setf trans '((+ . "and") (- . "subtract")))
((+ . "add") (- . "substract"))

(assoc '+ trans)
(+ . "add")

(assoc '* trans)
NIL

(defun our-assoc (key alist)
  (and (consp alist)
       (let (pair (car alist))
         (if (eql key (car pair))
             pair
             (our-assoc key (cdr alist))))))

;; Shortest path
(setf min '((a b c) (b c) (c d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (cdr queue)
                           (new-paths path node net))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
          (cdr (assoc node net))))

;; Garbages
(setf lst (list 'a 'b 'c))
(A B C)

(setf lst nil)
NIL
