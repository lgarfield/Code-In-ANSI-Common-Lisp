;; Array
(setf arr (make-array '(2 3) :initial-element nil))

(aref arr 0 0)
NIL

(setf (aref arr 0 0) 'b)
B

(aref arr 0 0)
B

#2a((b nil bil) (nil nil nil))

(setf *print-array* t)
T

arr
#2A((B NIL NIL) (NIL NIL NIL))

(setf vec (make-array 4 :initial-element nil))
#(NIL NIL NIL NIL)

(vector "a" 'b 3)
#("a" b 3)

(svref vec 0)
NIL

;; Binary Search
(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (let ((range (- end start)))
    (if (zerop range)
        (if (eql obj (aref vec start))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end)
                    obj)))))))

(format t "~A~%" (subseq vec start (+ end 1)))

;; Strings and Characters
(sort "elbow" #'char<)
"Below"

(aref "abc" 1)
#\b

(char "abc" 1)
#\b

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)
MerKin

(equal "fred" "fred")
T

(equal "fred" "Fred")
NIL

(string-equal "fred" "Fred")
T

(format nil "~A or ~A" "truth" "dare")
"truth or dare"

(concatenate 'string "not " "to worry")
"not to worry"

;; Sequences
(mirror? "abba")
T

(elt '(a b c) 1)
B

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
             ((or (> forward back)
                  (not (eql (elt s forward)
                            (elt s back))))
              (> forward back))))))

(position #\a "fantasia")
1

(position #\a "fantasia" :start 3 :end 5)
4

(position #\a "fantasia" :from-end t)
7

(position 'a '((c d) (a b)) :key #'car)
1

(position '(a b) '((a b) (c d)))
NIL

(position '(a b) '((a b) (c d)) :test #'equal)
0

(position 3 '(1 0 5 7) :test #'<)
2

(defun second-word (str)
  (let ((p1 (+ position #\ str 1)))
    (subseq str p1 (position #\ str :start p1))))

(second-word "Form follows function")
"follows"

(position-if #'oddp '(2 3 4 5))
1

(find #\a "cat")
#\a

(find-if #'characterp "ham")
#\h

(find-if #'(lambda (x)
             (eql (car x) 'complte))
         lst)

(find 'complete lst :key #'car)

(remove-duplicates "abracadabra")
"cdbra"

(reduce #'fn '(a b c d))

(fn (fn (fn 'a 'b) 'c ) 'd)

(reduce #'intersection '((b r a d 's) (b a d) (c a t)))
(A)

;;Parsing Dates
(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if #'(lambda (c)
                                   (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(tokens "ab12 3cde.f" #'alpah-char-p 0)
("ab" "cde" "f")

(parse-date "16 Aug 1980")
(16 8 1980)

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
                     :test #'string-equal)))
    (if p
        (+ p 1)
        nil)))

(defun read-integer (str)
  (if (every #'digit-char-p str)
      (let ((accum 0))
        (dotimes (pos (length str))
          (setf accum (+ (* accum 10)
                         (digit-char-p (char str pos)))))
        accum)
      nil))

;; Structures
(defun block-height (b) (svref b 0))

(defstruct point
  x
  y)

(setf p (make-pint :x 0 :y 0))
#S (POINT X 0 Y 0)

(point-x p)
0

(setf (point-y p) 2)
2

p
#S (PINTX X 0 Y 2)

(point-p p)
T

(typep p "point")
T

(defstruct polemic
  (type (progn
          (format t "What kind of polemic was it? ")
          (read)))
  (effect nil))

(make-polemic)
What kind of polemic was it? scathing
#S (POLEMIC :TYPE SCATHING :EFFECT NIL)

(defstruct (point (:conc-name p)
                  (:print-function print-point))
  (x 0)
  (y 0))

(defunc print-point (p stream depth)
  (format stream "#<~A, ~A>" (px p) (py p)))

(make-point)
#<0,0>

;; Binary Search Tree
(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-insert obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-insert obj (node-r bst) <)
                 :l (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (bst-find obj (node-l bst) <)
                (bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))

(setf nums nil)
NIL

(dolist (x '(5 8 4 2 1 9 6 7 3))
  (setf nums (bst-insert x nums #'<)))
NIL

(bst-find 12 nums #'<)
NIL

(bst-find 4 nums #'<)
#<4>

(bst-min nums)
#<1>

(bst-max nums)
#<9>

(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            (percolate bst)
            (if (funcall < obj elt)
                (make-node
                 :elt elt
                 :l (bst-remove obj (node-l bst) <)
                 :r (node-r bst))
                (make-node
                 :elt elt
                 :r (bst-remove obj (node-r bst) <)
                 :l (node-l bst)))))))

(defun percolate (bst)
  (cond ((null (node-l bst))
         (if (null (node-r bst))
             nil
             (rperc bst)))
        ((null (node-r bst)) (lperc bst))
        (t (if (zerop (random 2))
               (lperc bst)
               (rperc bst)))))

(defun rperc (bst)
  (make-node :elt (node-elt (node-r bst))
             :l (node-l bst)
             :r (percolate (node-r bst))))

(setf nums (bst-remove 2 nums #'<))
#<5>

(bst-find 2 nums #'<)
NIL

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))

(bst-traverse #'princ nums)

;; Hase Table
(setf ht (make-hash-table))
#<Hash-Table BF0A96>

(gethash 'color ht)
NIL
NIL

(setf (gethash 'color ht) 'red)
RED

(gethash 'color ht)
RED
T

(setf bug (make-hash-table))
#<Hash-Table BF4C36>

(push "Doesn't take keyword arguments."
      (gethash #'our-member bugs))
("Doesn't take keyword arguemnts.")

(setf fruit (make-hash-table))
#<Hash-Table BFDE76>

(setf (gethash 'apricot fruit) t)
T

(gethash 'apricot fruit)
T
T

(remhash 'apricot fruit)
T

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)
GIANT

(maphash #'(lambda (k v)
             (format t "~A = ~A~%" k v))
         ht)
SHAPE = SPHERICAL
SIZE = GIANT
COLOR = RED
NIL

(make-hash-table :size 5)

(setf writers (make-hash-table :test #'equal))
#<Hast-Table C005E6>

(setf (gethash '(ralph waldo emerson) writers) t)
T
