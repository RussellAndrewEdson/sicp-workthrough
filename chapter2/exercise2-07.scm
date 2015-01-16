;;; SICP Exercise 2.7
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise we specify the implementation of the interval
;;; abstraction for the interval arithmetic exercise.

; We have that the definition of the interval constructor is as follows:

(define (make-interval a b) (cons a b))

; Then assuming that we have a as the lower-bound and b as the upper-bound,
; we can write the selectors easily:

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))


; A quick test:

; The interval (-1,4)
(upper-bound (make-interval -1 4))
;> 4

(lower-bound (make-interval -1 4))
;> -1