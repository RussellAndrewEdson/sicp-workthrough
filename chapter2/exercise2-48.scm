;;; SICP Exercise 2.48
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used our vector representation from Exercise 2.46
;;; to make our representation for directed line segements.

; We have our vector constructors and selectors from Exercise 2.46 here:

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


; Now for our segement constructor 'make-segment', we simply take in the
; pair of vectors that define the end-points. We'll take the first vector
; given to be the start of the segment, and the second vector will mark the
; end of the segment. Then we'll just cons them together:

(define (make-segment start end)
  (cons start end))

; Then our selectors are simply 'car' and 'cdr', as before.

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


; As always, we'll test these. We'll define a line from (1,2) to (3,4)
; using our vector and segment implementations:

(define line (make-segment (make-vect 1 2)
                           (make-vect 3 4)))

; Our starting coordinates should be x=1, y=2:

(xcor-vect (start-segment line))
;> 1

(ycor-vect (start-segment line))
;> 2

; And our end coordinates should be x=3, y=4.

(xcor-vect (end-segment line))
;> 3

(ycor-vect (end-segment line))
;> 4