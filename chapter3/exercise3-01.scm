;;; SICP Exercise 3.1
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wrote a procedure to generate accumulators.


; We want a procedure 'make-accumulator' that takes in an initial value,
; and returns a procedure that takes a value and adds it to a running
; sum. As given in the book, we can do this (in an encapsulated way) by
; using a local variable to keep track of the sum at each point.
;
; Now recall that procedure parameters are themselves local variables,
; and so we can simply modify the parameter value (which we'll call 
; 'sum') to get what we want. We'll then return the new value.

(define (make-accumulator sum)
  (lambda (value)
    (set! sum (+ sum value))
    sum))

; And then our procedure works as specified. Notice that the second
; accumulator has its own unique state.

(define A (make-accumulator 5))

(A 10)
;> 15
  
(A 10)
;>25

(define B (make-accumulator 2))

(B 1)
;> 3

(A 32)
;> 57

(B 13)
;> 16