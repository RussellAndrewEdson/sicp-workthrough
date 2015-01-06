;;; SICP Exercise 1.39
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us using our cont-frac procedure from Exercise 1.37
;;; to define a procedure that computes an approximation to the tangent
;;; function using Lambert's continued fraction representation.


; We are given that Lambert's continued fraction representation for the
; tangent function is as follows:

;; tan x = x/(1 - (x^2/(3 - (x^2 /(5 - ...))...)

; ... where the numerator n values are x, x^2, x^2, x^2, ...
; and the d values are 1,3,5,7,9,... (ie. the odd numbers 2*n-1 if
; n is the index of the value in the sequence.)

; Note that we are subtracting successive fractions here, which we
; can take into account by making all of the n values negative 
; (except for the first one.)

; So we'll bring in our cont-frac procedure from Exercise 1.37:

(define (cont-frac n d k)
  (define (frac-iter n d count frac)
    (if (= count 0)
        frac
        (frac-iter n
                   d
                   (- count 1)
                   (/ (n count) (+ (d count) frac)))))
  (frac-iter n d k 0))


; Then we can easily define the tangent function approximation
; as a procedure using cont-frac as follows:

(define (tan-cf x k)
  (cont-frac (lambda (k) (if (= k 1)
                             x
                             (- (* x x))))
             (lambda (k) (- (* 2 k) 1))
             k))


; Sure enough, this function appears to work perfectly.
; We can use the already defined tan function for comparison.

(tan 1)
;> 1.5574077246549023

(tan-cf 1.0 100)
;> 1.557407724654902

(tan 5)
;> -3.380515006246586

(tan-cf 5.0 1000)
;> -3.3805150062465867