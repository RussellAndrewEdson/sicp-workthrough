;;; SICP Exercise 2.16
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we summarised why the algebraically equivalent
;;; expressions led to different answers in the interval-arithmetic system,
;;; and discussed how we might go about fixing this issue.


; For the sake of examples, we'll bring in all of the interval-arithmetic
; code we had by the end of Exercise 2.15:

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percentage)
  (let ((tolerance (* center percentage (/ 1 100))))
    (make-center-width center tolerance)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

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

(define (interval-contains? x interval)
  (<= (lower-bound interval) x (upper-bound interval)))

(define (div-interval x y)
  (if (interval-contains? 0 y)
      (error "Interval contains zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (print-interval i)
  (newline)
  (display (center i))
  (display " ")
  (display (percent i))
  (display "%"))


; So as we saw in Exercises 2.14 and 2.15, the problem for algebraically
; equivalent expressions is that they may have different numbers of references
; to the same variable. This is fine when we're dealing with regular numbers
; (eg. 2x-x = x with no problems in most cases.) However when we are dealing
; with intervals, we have the error (or tolerance) to consider.

; Let's quickly look at our 2x-x = x example, and suppose that x represents an
; uncertain number: for example, take x = 12 with 5% tolerance.

; Then when we multiply by 2 (or in terms of intervals, the interval (2,2)),
; then we get 2x = 24 with 5% tolerance.

; But what happens when we subtract x again? We move the center back to where
; we started, sure. But now we've actually added error -- x has an error of
; 5%, so by subtracting we are, in effect, adding this error to both sides
; of our 2x interval! So we end up with 15% error, as demonstrated below.

(define x (make-center-percent 12 5))
(print-interval x)
;> 12 5%

(define two-x (mul-interval (make-interval 2 2) x))
(print-interval two-x)
;> 24 5%

(print-interval (sub-interval two-x x))
;> 12 15%

; As we explained before, this happens because the arithmetic system doesn't
; take into account that the two references for x are the same (uncertain)
; number. So when it sees an expression for 2x-x, it evaluates the bounds of
; the resulting interval as follows:
;   - the lower bound: From the 2x term, we can get as low as 24-5%, or 22.8,
;     which happens when the number x is as small as possible.
;     Then for the -x term, we'll take x to be 12+5% this time, ie. 12.6, that
;     occurs when x takes its maximum value.
;     So our lower bound is 22.8-12.6 = 10.2, or 12-15%.
;
;   - the upper bound: From the 2x term, we can go as high as 24+5% = 25.2,
;     when x is as large as possible.
;     For the -x term, we'll take x to be as low as possible, ie. 12-5%, or
;     11.4.
;     Then our upper bound is 25.2-11.4 = 13.8, or 12+15%.


; So the main problem here is that the interval arithmetic system doesn't
; take into account that all of the references to the variable x are the
; same number throughout the calculation. To fix this, we would want all of
; the x's in the equation to refer to the same number, but at the same time
; we want all of the x's to refer to an -interval- of possible numbers when
; we think about the equation as an abstract expression built out of our
; interval arithmetic primitives.
;
; That said, getting a system to do something like this is quite difficult,
; and might not even be possible really. 
;
; (Probably a bit too difficult for a 2nd-chapter exercise, at any rate.)


; So what do we have to fall back on? Well, if we can rewrite an equation
; in such a way that no variables are repeated, then we're golden. Of course,
; you probably wouldn't have to look too far to find an equation where this 
; is not possible. So this is indeed quite a shortcoming for the 
; interval-arithmetic system.