;;; SICP Exercise 2.11
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we rewrote the mul-interval procedure by
;;; breaking it up into nine cases so we don't need to do as much
;;; multiplication.

; We have the original mul-interval procedure as follows:

;;  (define (mul-interval x y)
;;    (let ((p1 (* (lower-bound x) (lower-bound y)))
;;          (p2 (* (lower-bound x) (upper-bound y)))
;;          (p3 (* (upper-bound x) (lower-bound y)))
;;          (p4 (* (upper-bound x) (upper-bound y))))
;;      (make-interval (min p1 p2 p3 p4)
;;                     (max p1 p2 p3 p4))))


; Now we can break this up into cases by checking the
; signs of the endpoints of the intervals.

; Let [a,b], [c,d] be the intervals to be multiplied.
; Then we can tabulate the different combinations for the
; signs of the endpoints (including the resulting interval).
;
; Of the sixteen different combinations, only nine are actually
; possible (we exclude cases where b < a and d < c.)
;
; The intervals can then be directly determined by figuring out
; what would yield the minimum and maximum product in each case.

;  a  | b  | c  | d  | multiplication interval
; ---------------------------------------------
;  +  | +  | +  | +  | (a*c, b*d)
;  +  | +  | +  | -  | Not possible. (d < c)
;  +  | +  | -  | +  | (b*c, b*d)
;  +  | +  | -  | -  | (b*c, a*d)
;  +  | -  | +  | +  | Not possible. (b < a)
;  +  | -  | +  | -  | Not possible. (b < a and d < c)
;  +  | -  | -  | +  | Not possible. (b < a)
;  +  | -  | -  | -  | Not possible. (b < a)
;  -  | +  | +  | +  | (a*d, b*d)
;  -  | +  | +  | -  | Not possible. (d < c)
;  -  | +  | -  | +  | (min{a*d, b*c}, max{a*c, b*d})
;  -  | +  | -  | -  | (b*c, a*c)
;  -  | -  | +  | +  | (a*d, b*c)
;  -  | -  | +  | -  | Not possible (d < c)
;  -  | -  | -  | +  | (a*d, a*c)
;  -  | -  | -  | -  | (b*d, a*c)

; So we can see the nine cases here, with all of them only
; requiring two multiplications (except for the -,+,-,+ case,
; which requires four multiplications and a call to the min
; and max procedures.)

; So we can rewrite our mul-interval procedure to test the
; signs of the endpoints. We'll also define a sign procedure
; to return 1 or -1 depending on the sign of the given number,
; which should make our procedure slightly more readable.

(define (sign x)
  (if (< x 0)
      -1
      1))

(define (mul-interval x y)
  (define (endpoint-signs? s1 s2 s3 s4)
    (and (= (sign (lower-bound x)) s1)
         (= (sign (upper-bound x)) s2)
         (= (sign (lower-bound y)) s3)
         (= (sign (upper-bound y)) s4)))
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((endpoint-signs?  1  1  1  1) (make-interval (* a c) (* b d)))
          ((endpoint-signs?  1  1 -1  1) (make-interval (* b c) (* b d)))
          ((endpoint-signs?  1  1 -1 -1) (make-interval (* b c) (* a d)))
          ((endpoint-signs? -1  1  1  1) (make-interval (* a d) (* b d)))
          ((endpoint-signs? -1  1 -1  1) (make-interval (min (* a d) (* b c))
                                                        (max (* a c) (* b d))))
          ((endpoint-signs? -1  1 -1 -1) (make-interval (* b c) (* a c)))
          ((endpoint-signs? -1 -1  1  1) (make-interval (* a d) (* b c)))
          ((endpoint-signs? -1 -1 -1  1) (make-interval (* a d) (* a c)))
          ((endpoint-signs? -1 -1 -1 -1) (make-interval (* b d) (* a c)))
          (else (error "Malformed interval passed" x y)))))


; We can bring in the constructor and selectors to test each of these cases:

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define pos-pos (make-interval 2 5))
(define pos-neg (make-interval 6 -2))
(define neg-pos (make-interval -4 2))
(define neg-neg (make-interval -7 -3))

; (2,5)*(2,5) = (min{4,10,25}, max{4,10,25})
(mul-interval pos-pos pos-pos)
;> {mcons 4 25}

; (2,5)*(6,-2) is malformed (6 > -2)
;(mul-interval pos-pos pos-neg)
;> Error: Malformed interval passed (2 . 5) (6 . -2)

; (2,5)*(-4,2) = (min{-8,4,-20,10}, max{-8,4,-20,10})
(mul-interval pos-pos neg-pos)
;> {mcons -20 10}

; (2,5)*(-7,-3) = (min{-14,-6,-35,-15}, max{-14,-6,-35,-15})
(mul-interval pos-pos neg-neg)
;> {mcons -35 -6}

; (6,-2)*(2,5) is malformed (6 > -2)
;(mul-interval pos-neg pos-pos)
;> Error: Malformed interval passed (6 . -2) (2 . 5)

; (6,-2)*(6,-2) is malformed (6 > -2)
;(mul-interval pos-neg pos-neg)
;> Error: Malformed interval passed (6 . -2) (6 . -2)

; (6,-2)*(-4,2) is malformed (6 > -2)
;(mul-interval pos-neg neg-pos)
;> Error: Malformed interval passed (6 . -2) (-4 . 2)

; (6,-2)*(-7,-3) is malformed (6 > -2)
;(mul-interval pos-neg neg-neg)
;> Error: Malformed interval passed (6 . -2) (-7 . -3)

; (-4,2)*(2,5) = (min{-8,-20,4,10}, max{-8,-20,4,10})
(mul-interval neg-pos pos-pos)
;> {mcons -20 10}

; (-4,2)*(6,-2) is malformed (6 > -2)
;(mul-interval neg-pos pos-neg)
;> Error: Malformed interval passed (-4 . 2) (6 . -2)

; (-4,2)*(-4,2) = (min{16,-8,4}, max{16,-8,4})
(mul-interval neg-pos neg-pos)
;> {mcons -8 16}

; (-4,2)*(-7,-3) = (min{28,12,-14,-6}, max{28,12,-14,-6})
(mul-interval neg-pos neg-neg)
;> {mcons -14 28}

; (-7,-3)*(2,5) = (min{-14,-35,-6,-15}, max{-14,-35,-6,-15})
(mul-interval neg-neg pos-pos)
;> {mcons -35 -6}

; (-7,-3)*(6,-2) is malformed (6 > -2)
;(mul-interval neg-neg pos-neg)
;> Error: Malformed interval passed (-7 . -3) (6 . -2)

; (-7,-3)*(-4,2) = (min{28,-14,12,-6}, max{28,-14,12,-6})
(mul-interval neg-neg neg-pos)
;> {mcons -14 28}

; (-7,-3)*(-7,-3) = (min{49,21,9}, max{49,21,9})
(mul-interval neg-neg neg-neg)
;> {mcons 9 49}

; So our new mul-interval procedure seems to work properly.
