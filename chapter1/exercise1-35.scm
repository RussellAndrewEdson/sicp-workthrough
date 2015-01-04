;;; SICP Exercise 1.35
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us showing that the Golden Ratio (phi) is the fixed
;;; point of the mapping x -> 1 + 1/x, and then computing an approximation
;;; to the Golden Ratio by using the fixed-point procedure from the book.


; Recall that phi = (1 + sqrt(5))/2.
; Applying the map x -> 1 + 1/x to phi gives us the following:

;;   1 + 1/phi = 1 + 2/(1 + sqrt(5))
;;             = (1 + sqrt(5) + 2) / (1 + sqrt(5))
;;             = 2*(1 + sqrt(5) + 2) / 2*(1 + sqrt(5))
;;             = (2 + 2*sqrt(5) + 4) / 2*(1 + sqrt(5))
;;             = (1 + 2*sqrt(5) + 5) / 2*(1 + sqrt(5))
;;             = (1 + sqrt(5))^2 / 2*(1 + sqrt(5))
;;             = (1 + sqrt(5)) / 2
;;             = phi

; So 1 + 1/phi = phi, and we have that phi is a fixed point of this
; transformation.


; In that case, let's compute an approximation to phi using the fixed-point
; procedure defined in the book (included below):

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; We can compute the approximation with a first guess of 1.0 in this case.

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;> 1.6180327868852458

; Compared to the calculation using the formula below, we can see that this
; approximation is definitely within the given tolerance.

(/ (+ 1 (sqrt 5)) 2)
;> 1.618033988749895