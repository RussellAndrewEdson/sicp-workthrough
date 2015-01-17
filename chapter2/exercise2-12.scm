;;; SICP Exercise 2.12
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we defined constructors and selectors that deal
;;; with the intervals in terms of their center and a percentage tolerance.

; The previously defined constructors and selectors are as follows:

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))


; We define a constructor make-center-percent that constructs an
; interval from a given center and percentage tolerance.
; (eg. given "x units 5%", we want an interval defined by the midpoint
; x, like so: (x - 5%, x + 5%). )
;
; We can actually define the constructor in terms of the make-center-width
; constructor as follows:

(define (make-center-percent center percentage)
  (let ((tolerance (* center percentage (/ 1 100))))
    (make-center-width center tolerance)))


; Now we also need a selector percent that returns the percentage for
; the given interval. Given an interval, we find the center (using the
; already-defined center selector), and then find the ratio of the
; width of the interval to the center. Multiplying this number by 100
; gives us our required percentage.
;
; eg. Consider the interval (1.5, 1.9) with center 1.7. We want to write
; 1.9 = 1.7 + 1.7*p for some value p, where p is the fraction of 1.7 we
; multiply by to get to the endpoint. Rearranging gives us p = 0.2/1.7,
; which is exactly the ratio of the width to the center. We then multiply
; p by 100 to make it a percentage (of 1.7).

(define (percent i)
  (* (/ (width i) (center i)) 100))


; We can test these out using the examples from the book.

; 6.8-ohm 10% is an interval between 6.12 and 7.48:
(define interval-one (make-center-percent 6.8 10))

(lower-bound interval-one)
;> 6.12

(upper-bound interval-one)
;> 7.4799999999999995

; 4.7-ohm 5% is an interval between 4.465 and 4.935:
(define interval-two (make-interval 4.465 4.935))

(center interval-two)
;> 4.699999999999999

(percent interval-two)
;> 4.999999999999998
