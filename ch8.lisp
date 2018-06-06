;; Symbol Names
(symbol-name 'abc)

(eql 'abc 'ABC)
T

(Car '(a b c))
A

(list '|Lisp 1.5| '|| '|abc| '|ABC|)

(symbol-name '|a b c|)

;; Property Lists
(get 'alizarin 'color)

(setf (get 'alizarin 'color) 'red)
RED

(get 'alizarin 'color)
RED

(setf (get 'alizarin 'transparency) 'high)
HIGH

(symbol-plist 'alizarin)
(TRANSPARENCY HIGH COLOR RED)

;; Symbols Are Big

;; Create Symbols
(intern "RANDOM-SYMBOL")
RANDOM-SYMBOL
NIL

;; Multiple Packages
(defpackage "MY-APPLICATION"
  (:use "COMMOM-LISP" "MY-UTILITIES")
  (:nickname "APP")
  (:export "WIN" "LOSE" "DRAW"))

(in-package my-application)

;; Keywords
(defun noise (animal)
  (case animal
    (:dog :woof)
    (:cat :meow)
    (:pig :oink)))

;; Symbols and Variables

;; Random Text
(defparameter *word* (make-hash-table :size 10000))

(defconstant maxwod 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))
          (pos 0))
      (do ((c (read-char s nil :eof)
              (read-char s nil :eof)))
          ((eql c :eof))
        (if (or (alpha-char-p c) (char= c #\'))
            (progn
              (setf (aref buffer pos) c)
              (incf pos))
            (progn
              (unless (zerop pos)
                (see (intern (string-downcase
                              (subseq buffer 0 pos))))
                (setf pos 0))
              (let ((p (punc c)))
                (if p (see p)))))))))

(defun punc (c)
  (case c
    (#\. '|.|) (#\, '|,|) (#\; '|;|)
    (#\! '|!|) (#\? '|?|) ))

(let ((prev '|.|))
  (defun see (symb)
    (let ((pair (assoc symb (gethash prev *word*))))
      (if (null pair)
          (push (cons symb 1) (gethash prev *words*))
          (incf (cdr pair))))
    (setf prev symb)))

((|sin| . 1) (|wide| . 2) (|sights| . 1))

(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
      (terpri)
      (let ((next (random-text prev)))
        (format "~A " next)
        (generate-text (1- n) next))))

(defun random-text (prev)
  (let* ((choices (gethash prev *words*))
         (i (random (reduce #'+ choices
                            :key #'cdr))))
    (dolist (pair choices)
      (if (minusp (decf i (cdr pair)))
          (return (car pair))))))
