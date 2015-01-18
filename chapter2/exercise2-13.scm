;;; SICP Exercise 2.13
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we showed that there is a simple formula for the 
;;; approximate percentage tolerance of the product of two intervals,
;;; provided that the percentage tolerances of the factors were
;;; sufficiently small.


; We are allowed to simplify the problem here by assuming that we only
; have positive values (for the centers and tolerances).

; First, recall that multiplication for interval arithmetic is defined
; as follows:
;   (a,b) * (c,d) = (min{ac,ad,bc,bd}, max{ac,ad,bc,bd})

; ..and an interval with center c and percentage tolerance p is written
; in terms of its endpoints as:
;   ( c-(p/100)*c, c+(p/100)*c ) = ( c(1-p/100), c(1+p/100) )


; So let's multiply two intervals (a,b) and (c,d), and let (a,b) have
; center c1 and percentage tolerance p1, and (c,d) has center c2 and
; percentage tolerance p2.

; Then we have:
;   a = c1(1-p1/100), b = c1(1+p1/100)
;   c = c2(1-p2/100), d = c2(1+p2/100).

; And we can find expressions for ac, ad, bc, bd:
;  ac = c1c2(1 - p2/100 + p1/100 + p1p2/100^2) = c1c2(1 - (p1 + p2 - (p1p2/100))/100)
;  ad = c1c2(1 + p2/100 - p1/100 - p1p2/100^2) = c1c2(1 - (p1 - p2 + (p1p2/100))/100)
;  bc = c1c2(1 - p2/100 + p1/100 - p1p2/100^2) = c1c2(1 + (p1 - p2 - (p1p2/100))/100)
;  bd = c1c2(1 + p2/100 + p1/100 + p1p2/100^2) = c1c2(1 + (p1 + p2 + (p1p2/100))/100)


; Now assume that the factor percentage tolerances are sufficiently small 
; that the term (p1p2/100) is negligible. Then we can approxmate ac, ad,
; bc, bd as follows:

;  ac = c1c2(1 - (p1 + p2)/100)
;  ad = c1c2(1 - (p1 - p2)/100)
;  bc = c1c2(1 + (p1 - p2)/100)
;  bd = c1c2(1 + (p1 + p2)/100)

; Now since we simplified the problem so that c1, c2, p1, p2 are all positive,
; then the minimum of these obviously occurs at ac, and the maximum occurs 
; at bd. So our product is the following interval:

; (a,b) * (c,d) = ( c1c2(1 - (p1+p2/100)), c1c2(1 + (p1+p2/100)) ),

; and this is approximately the interval with center c1c2 and percentage
; tolerance p1 + p2, ie. the sum of the percentage tolerance of the factors!


; We can bring in the interval arithmetic code and try some examples:

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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


; Testing the intervals (5.0 %2) and (12.0 %1):
(define interval-one (make-center-percent 5.0 2))
(define interval-two (make-center-percent 12.0 1))

(center (mul-interval interval-one interval-two))
;> 60.012

(percent (mul-interval interval-one interval-two))
;> 2.9994001199759883
; approximately 3, which is the sum of the factor tolerances.


(define interval-three (make-center-percent 32.0 7))
(define interval-four (make-center-percent 17.0 3))

(center (mul-interval interval-three interval-four))
;> 545.1424

(percent (mul-interval interval-three interval-four))
;> 9.979044007584086
; approximately 10, which is the sum of the factor tolerances
; (but we can see that the approximation gets worse as we
; increase the percentages.)
