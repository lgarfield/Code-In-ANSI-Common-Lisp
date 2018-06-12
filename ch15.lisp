;; The Aim
(parent donald nancy)

(<- head body)

(<- (child ?x ?y) (parent ?y ?x))

(<- (father ?x ?y) (and (parent ?x ?y) (male ?x)))

(<- (daughter ?x ?y) (and (child ?x ?y) (female ?x)))

;; matching
(p ?x ?y c ?x)
(p a b c a)

(p ?x b ?y a)
(p ?y b c a)

(defun match (x y &optional binds)
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    (t
     (when (and (consp x) (consp y))
       (multiple-value-bind (b2 yes)
           (match (car x) (car y) binds)
         (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (let ((b (assoc x binds)))
    (if b
        (or (binding (cdr b) binds)
            (cdr b)))))

(match '(p a b c a) '(p ?x ?y c ?x))
((?Y . B) (?X . A))
T

(match '(p ?x b ?y a) '(p ?y b c a))
((?Y . C) (?X . ?Y))
T

(match '(a b c) '(a a a))
NIL

(match '(p ?x) '(p ?x))
NIL
T

(match '(p ?v b ?x d (?z ?z))
       '(p a ?w c /y (e e))
       ((?v . a) (?w . b)))
((?Z . E) (?Y . D) (?X . C) (?V . A) (?W . B))
T

(match '(?x a) '(?y ?y))
((?Y . A) (?X . ?Y))
T

;; Answering Queries
(parent donald nancy)

(parent ?x ?y)
(((?x . donald) (?y . nancy)))

(defvar *rules* (make-hash-table))

(defmacro <- (con &optional ant)
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))

(<- (parent donald nancy))
1

(<- (child ?x ?y) (parent ?y ?x))
1
