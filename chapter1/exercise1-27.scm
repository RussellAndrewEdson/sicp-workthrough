;;; SICP Exercise 1.27
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us demonstrating that the Carmichael numbers fool
;;; the Fermat test, by testing a^n = a mod n for all a < n on the 
;;; Carmichael numbers 561, 1105, 1729, 2465, 2821 and 6601.

; So we want a procedure that takes in a given number n, and tests
; whether a^n is congruent to a modulo n for every a in [1,n).

; First things first, we'll probably need the expmod procedure from 
; earlier:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x)
  (* x x))


; Now we can write the Fermat tester code as a recursive procedure that
; generates an iterative process in the usual way:

(define (passes-fermat-test? n)
  (define (fermat-test-iter a)
    (cond ((= a n) true)
          ((not (= (expmod a n n) a)) false)
          (else (fermat-test-iter (+ a 1)))))
  (fermat-test-iter 1))

; This procedure will check each of the numbers between 1 and n-1 inclusive
; and return false whenever one of those numbers causes the Fermat test
; to fail. If none do, then we'll end up iterating through 1,2,..., up to n, 
; at which point the given number would have passed all the tests.


; To convince ourselves that this code will work, we should test it on some
; simple examples (eg. primes like 5, 13, 19 should pass all the tests,
; and non-primes like 8, 12, 26 should fail at least one of them):

(passes-fermat-test? 5)
;> #t

(passes-fermat-test? 13)
;> #t

(passes-fermat-test? 19)
;> #t

(passes-fermat-test? 8)
;> #f

(passes-fermat-test? 12)
;> #f

(passes-fermat-test? 26)
;> #f

; So the output agrees with what we'd expect. Then running the procedure on
; the Carmichael numbers gives us the following:

(passes-fermat-test? 561)
;> #t

(passes-fermat-test? 1105)
;> #t

(passes-fermat-test? 1729)
;> #t

(passes-fermat-test? 2465)
;> #t

(passes-fermat-test? 2821)
;> #t

(passes-fermat-test? 6601)
;> #t

; So as you can see, the Carmichael numbers pass the Fermat test.


; ** But do they -fool- the Fermat test? For that to be true, they would
; have to actually not be prime (since we fully expect prime numbers to
; pass the test anyway!) Of course, this is worth checking. Fortunately
; we have the old prime? procedure that can verify this for us:

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


; So we check whether the given Carmichael numbers are prime:

(prime? 561)
;> #f

(prime? 1105)
;> #f

(prime? 1729)
;> #f

(prime? 2465)
;> #f

(prime? 2821)
;> #f

(prime? 6601)
;> #f

; So the numbers aren't prime, which means that the fact that they
; passed the Fermat test is actually a problem.