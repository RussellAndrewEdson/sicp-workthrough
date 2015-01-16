;;; SICP Exercise 2.8
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we define the subtraction procedure for the
;;; interval arithmetic exercise.

; We can reason that an interval will always look like (lower, upper).
; In that case, consider the subtraction of the intervals (a,b) and (c,d).

; Now the minimum value for the difference will actually occur when we
; subtract the lower bound of the first interval, a, from the -upper-
; bound of the second interval, d. Similarly, the maximum value occurs when
; we take the upper-bound b and subtract the lower-bound c.
; So our subtraction interval looks like (a-d, b-c).

; We can implement that as follows:

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


; We can pull in our constructors and selectors from Exercise 2.7
; for a quick test:

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))


; Subtracting intervals: (-5, -2) - (-1, 4) = (-9, -1)

(define test-interval (sub-interval (make-interval -5 -2)
                                    (make-interval -1 4)))

(car test-interval)
;> -9

(cdr test-interval)
;> -1
