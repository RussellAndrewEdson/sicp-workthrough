;;; SICP Exercise 1.21
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise were using the smallest-divisor procedure detailed
;;; in the book to find the smallest divisors of the numbers 199, 1999, 
;;; and 19999.

; The smallest-divisor procedure is as follows:

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))


; We simply pass it the numbers 199, 1999 and 19999 in turn.

(smallest-divisor 199)
;> 199

(smallest-divisor 1999)
;> 1999

(smallest-divisor 19999)
;> 7