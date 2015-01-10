;;; SICP Exercise 1.43
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we use the idea of function composition from
;;; Exercise 1.42 to write a procedure that repeatedly applies a
;;; function to an argument a given number of times.


; Our compose function from Exercise 1.42 is included below.

(define (compose f g)
  (lambda (x) (f (g x))))


; Using this, we can write a procedure that composes a
; function with itself n times. We do this by recursively
; composing the function f with itself; the base case simply
; returns the identity function.

(define (repeated f n)
  (cond ((= n 0) identity)
        (else (lambda (x) 
                ((compose f (repeated f (- n 1))) x)))))


; We can test the function with some examples:

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

((repeated square 2) 5)
;> 625

((repeated inc 7) 2)
;> 9