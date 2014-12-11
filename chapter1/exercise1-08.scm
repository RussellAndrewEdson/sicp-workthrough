;;; SICP Exercise 1.8
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we had to implement a cube-root procedure
;;; using the Newton's method approximation:
;;;    ((x/y^2) + 2y) / 3
;;;
;;; Similar to the square-root procedure we just had.

(define (cube-root x)
  (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
      guess
      (cube-root-iter (improve guess x)
                      x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x))


; The cube-root method appears to work as required.

(cube-root (cube 3))
;> 3.0000005410641766

(cube-root (cube 45))
;> 45.000000000077506

(cube-root (cube 122))
;> 122.00000000004441

(cube-root (cube 0.7))
;> 0.7000925372214241
