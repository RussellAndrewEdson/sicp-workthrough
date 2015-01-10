;;; SICP Exercise 1.42
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise we wrote a procedure that implemented the notion
;;; of function composition, (f o g)(x) = f(g(x)).


; We are given that the functions only have one argument each, so this
; is actually a really simple procedure definition:

(define (compose f g)
  (lambda (x) (f (g x))))


; We can even test it with the given example:
(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

((compose square inc) 6)
;> 49