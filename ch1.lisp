;; 1.1 new tools
(defun sum (n)
  (let ((s 0))
    (dotimes (i n s)
      (incf s i))))

(defun addn (n)
  #'(lambda (x)
      (+ x n)))
