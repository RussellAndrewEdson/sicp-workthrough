;;; SICP Exercise 2.9
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we looked at the width of an interval and
;;; the relationship between the widths of intervals and their
;;; arithmetic combinations.

; We'll bring in all of our interval arithmetic code here:

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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; We'll also define some example intervals that we can test with.
(define interval-one (make-interval -1 1))
(define interval-two (make-interval -4 -1))
(define interval-three (make-interval -2 6))
(define interval-four (make-interval 5 9))


; We can define the width of an interval with the following procedure:

(define (width z)
  (/ (- (upper-bound z) (lower-bound z))
     2))


; Now for addition, the width of the resulting interval is simply
; the sum of the widths of the intervals.
;
; (Why? Suppose we have the intervals (a,b) and (c,d). Then the widths
; of the intervals are (b-a)/2 and (d-c)/2 respectively.
;
; Then the addition of the intervals is (a+c,b+d) with a width of
; (b+d - (a+c))/2 = (b-a)/2 + (d-c)/2. )

; As an example:

(width interval-one)
;> 1

(width interval-two)
;> 1 1/2

(width (add-interval interval-one interval-two))
;> 2 1/2


; Similarly, the width of the subtraction interval is the sum of the
; widths of the two intervals used.
;
; (Let (a,b), (c,d) be the intervals as before, with widths (b-a)/2 and
; (d-c)/2. Then the subtracted interval is (a-d, b-c), which has width
; given by ((b-c) - (a-d))/2 = (b-a+d-c)/2 = (b-a)/2 + (d-c)/2. )

; Another example:

(width interval-three)
;> 4

(width interval-four)
;> 2

(width (sub-interval interval-three interval-four))
;> 6


; But the multiplication of two intervals doesn't have such a relationship
; between the widths.
;
; To show this, we just need to consider a possible function of the widths,
; w = f(w1, w2), where w1, w2 are the widths of the intervals and w is the
; width of the multiplied interval.
;
; We want to show that this function cannot exist; so we simply need to 
; show that we can fix w1, w2 (ie. find different intervals with the same
; widths) and yet end up with at least two different values for w.

; Let's keep things as simple as possible. Consider the following two
; intervals of width 1:

(define interval-five (make-interval -1 1))
(width interval-five)
;> 1

(define interval-six (make-interval 2 4))
(width interval-six)
;> 1

; And consider the following interval of width 2:

(define interval-seven (make-interval -8 -4))
(width interval-seven)
;> 2

; Now interval-five multiplied by interval-seven gives us the
; following interval width:

(width (mul-interval interval-five interval-seven))
;> 8

; And interval-six multiplied by interval-seven gives us this
; interval width:

(width (mul-interval interval-six interval-seven))
;> 12

; Here we have two intervals of the same width that give different
; widths when they multiply with the same interval. So we cannot 
; have a function that maps the widths of the two intervals to the
; width of the multiplication interval.


; We can do the exact same thing with division:

(width (div-interval interval-five interval-seven))
;> 0.25

(width (div-interval interval-six interval-seven))
;> 0.375

; The widths of interval-five and interval-six are the same, and 
; interval-seven obviously doesn't change its width between the
; calculations. And yet, we end up with different answers. So there
; cannot be a relation between the widths for division either.

