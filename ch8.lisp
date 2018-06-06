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
