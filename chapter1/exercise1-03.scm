;;; SICP Exercise 1.3
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise wanted us to define a procedure that takes three
;;; numbers as arguments and returns the sum of the squares of the
;;; two larger numbers. (We'll do this the hard way using cond, since
;;; we haven't seen the min/max procedures by this point.)

(define (sum-squares-of-largest-two x y z)
  (cond ((and (<= x y) (<= x z)) (+ (* y y) (* z z)))
        ((and (<= y x) (<= y z)) (+ (* x x) (* z z)))
        (else (+ (* x x) (* y y)))))


; The procedure appears to work as required.

(sum-squares-of-largest-two 1 2 3)
;> 13

(sum-squares-of-largest-two 1 0 -1)
;> 1

(sum-squares-of-largest-two 2 0 4)
;> 20

; It even works for doubling up. (Note that the 0-0-1 example doesn't
; work if we replace '<=' with '<' in the procedure definition.)

(sum-squares-of-largest-two 0 0 1)
;> 1

(sum-squares-of-largest-two 1 2 1)
;> 5

(sum-squares-of-largest-two 3 4 4)
;> 32
