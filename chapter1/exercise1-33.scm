;;; SICP Exercise 1.33
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us take the accumulate procedure of Exercise 1.32
;;; and generalise it further into a filtered-accumulate procedure that
;;; allows for filtering of the terms to be combined.

; The accumulate procedure (the iterative-process version) was as follows:

;;  (define (accumulate combiner null-value term a next b)
;;    (define (iter a result)
;;      (if (> a b)
;;          result
;;          (iter (next a)
;;                (combiner result (term a)))))
;;    (iter a null-value))


; Now we want to have a filter such that we only combine terms where
; a certain predicate is true. (Otherwise, we don't accumulate that term.)
;
; This is very similar to the structure of the accumulate procedure
; above, but with an additional check when we iterate.

(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))


; And it's as simple as that. As a quick example, we can use it to add
; up only those odd numbers between 1 and 10:

(define (inc x) 
  (+ x 1))

(filtered-accumulate odd? + 0 identity 1 inc 10)
;> 25
; 1+3+5+7+9 = 25.


; As a better example, suppose we bring in that prime testing code
; we had back in Exercise 1.22:

(define (prime? n)
  (= n (smallest-divisor n)))

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


; We can then express things like "the sum of the squares of the prime
; numbers in the interval [a,b]" using 'prime?' as the predicate:

(define (sum-of-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))


; So the sum of the prime squares in [1,10] is as follows:

(sum-of-prime-squares 1 10)
;> 88
; 1+4+9+25+49 = 88.


; Similarly, we can express the product of all the positive integers
; less than a given n that are relatively prime (or coprime) to n:

(define (product-coprime-integers n)
  (define (coprime? a)
    (= (gcd n a) 1))
  (filtered-accumulate coprime? * 1 identity 1 inc (- n 1)))


; So the product of the coprime integers less than 21 is as follows:

(product-coprime-integers 15)
;> 896896
; 1*2*4*7*8*11*13*14 = 896896.