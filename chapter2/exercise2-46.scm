;;; SICP Exercise 2.46
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise, we implemented the contructors and selectors for
;;; a data abstraction for vectors, and then used the abstraction to
;;; implement the basic vector operations.

; We'll choose to represent our vectors as a literal pair (ie. a cons cell.)
; So then our constructor and selectors are simply defined as follows:

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))


; Now for the vector addition operation, the result is a new vector that
; has its x-coordinate as the sum of the x-coordinates for the two operands,
; and similarly its y-coordinate is the sum of the y-coordinates for the
; two given vectors. 
;
; (Recall though that we want to only work in terms of our abstraction! 
; So we should write all of these operations in terms of our constructor
; and selectors.)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))


; Similarly, our vector subtraction procedure returns a new vector whose
; coordinates are the corresponding -differences- of the two given vectors:

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))


; Finally, the scalar multiplication operation takes in a vector, and
; returns a new vector whose coordinates are the coordinates of the given
; vector multiplied by the scalar:

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


; We can test these opertions with some simple examples.

(define v1 (make-vect 3 4))
(define v2 (make-vect -1 6))
(define s1 5)
(define s2 -2)

; Adding v1 and v2 should give us (2,10)
(add-vect v1 v2)
;> {mcons 2 10}

; Subtracting v1 from v2 should give us (-4,2)
(sub-vect v2 v1)
;> {mcons -4 2}

; Scaling v1 by 5 should give us (15,20)
(scale-vect s1 v1)
;> {mcons 15 20}

; Scaling v2 by -2 should give us (2,-12)
(scale-vect s2 v2)
;> {mcons 2 -12}