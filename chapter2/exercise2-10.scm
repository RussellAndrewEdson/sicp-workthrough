;;; SICP Exercise 2.10
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to modify the interval division
;;; procedure to error when the dividing interval contains 0.

; The basic idea of this interval arithmetic is that we're basically
; operating on all of the values in the first interval simultaneously
; with all of the values in the second interval.
;
; eg. for adding the intervals [a,b] and [c,d], we want to be able to
; pick any number in [a,b], add it to any number in [c,d], and have
; that number appear in the interval [a,b]+[c,d] somewhere.

; So given this, what does it mean to divide by an interval that spans
; zero? (ie. for [a,b] / [c,d], 0 exists somewhere in [c,d].)
; A division by zero is undefined in normal arithmetic for most cases
; anyway, and even if we define a division by zero to return infinity,
; now what? As we limit toward a division by zero, we've spanned
; the entire number line. Seems pretty pointless to return 
; [-infinity,infinity] as a result every time.


; To get around this, we'll have the division procedure simply return 
; an error if given a [c,d] interval that spans 0. We'll do this
; by defining a useful auxiliary procedure, interval-contains?

(define (interval-contains? x interval)
  (<= (lower-bound interval) x (upper-bound interval)))

(define (div-interval x y)
  (if (interval-contains? 0 y)
      (error "Interval contains zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))


; We can test this by bringing in the rest of the interval
; arithmetic code:

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


; Testing for interval / zero:

(div-interval (make-interval 1 5) (make-interval -5 2))
;> Error: Interval contains zero (-5 . 2)
