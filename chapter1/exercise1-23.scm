;;; SICP Exercise 1.23
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we want to make the smallest-divisor procedure
;;; more efficient by not checking any larger even numbers than 2. We
;;; will then check this by comparing the run-time with the same primes
;;; that we found in Exercise 1.22.

; First we make a 'next' procedure that returns 3 when the input 
; is 2, and increments the input by 2 otherwise.

(define (next n)
  (if (= n 2) 
      3
      (+ n 2)))


; We'll use this procedure in the definition of the smallest-divisor
; instead of the increment (in the find-divisor procedure):

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))


; We'll bring in the timed-prime-test procedure too (along with
; all of its ancillary procedures):

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))


; Now we'll run the test for each of the primes we found in Exercise 1.22.
;
; (Note that I've run the tests for each individually, since doing them in 
; sequence in Exercise 1.22 seemed to speed it up by a massive margin for the
; smaller prime checks, which didn't make for much of a fair comparison. 
; So for the following tests I've basically cleared the environment 
; before running each one.)

;; 1009 *** 157
;; 1013 *** 181
;; 1019 *** 257
;; 10007 *** 457
;; 10009 *** 326
;; 10037 *** 226
;; 100003 *** 355
;; 100019 *** 354
;; 100043 *** 341
;; 1000003 *** 952
;; 1000033 *** 764
;; 1000037 *** 935


; We'll run the new (and improved?) timed-prime-test for each of these primes:
; (These tests were run individually too.)

;; (timed-prime-test 1009)
;> 1009 *** 321

;; (timed-prime-test 1013)
;> 1013 *** 272

;; (timed-prime-test 1019)
;> 1019 *** 194

;; (timed-prime-test 10007)
;> 10007 *** 272

;; (timed-prime-test 10009)
;> 10009 *** 187

;; (timed-prime-test 10037)
;> 10037 *** 201

;; (timed-prime-test 100003)
;> 100003 *** 274

;; (timed-prime-test 100019)
;> 100019 *** 341

;; (timed-prime-test 100043)
;> 100043 *** 335

;; (timed-prime-test 1000003)
;> 1000003 *** 650

;; (timed-prime-test 1000033)
;> 1000033 *** 847

;; (timed-prime-test 1000037)
;> 1000037 *** 631


; Side-by-side for comparison, we have the following:

;   n        |  Time (+ test-divisor 1)  |  Time (next test-divisor)  |  Ratio
; ------------------------------------------------------------------------------
; 1009       | 157                       | 321                        | 0.489
; 1013       | 181                       | 272                        | 0.665
; 1019       | 257                       | 194                        | 1.325
; 10007      | 457                       | 272                        | 1.680
; 10009      | 326                       | 187                        | 1.743
; 10037      | 226                       | 201                        | 1.124
; 100003     | 355                       | 274                        | 1.296
; 100019     | 354                       | 341                        | 1.038
; 100043     | 341                       | 335                        | 1.018
; 1000003    | 952                       | 650                        | 1.465
; 1000033    | 764                       | 847                        | 0.902
; 1000037    | 935                       | 631                        | 1.482

; Now the average of the ratios is about 1.186 (and the median value is 1.210).
; This is quite different from the expectation that the modified version runs
; twice as fast!
;
; However, notice the overhead that we have with the modification. Instead of
; a simple increment procedure, we're now calling a procedure that has an if
; check for a 2, as well as a call to an increment procedure when that check
; fails (which will be most of the time, by design.) So with this in mind, the
; ratios that we're seeing here kind of make sense. Sure, we're skipping every
; second number, but for the numbers we do work with, we're doing that much
; more work in the procedure calls. The numbers aren't quite big enough for 
; the tradeoff to be "worth it" yet.


; We can see the difference more readily for larger prime numbers though
; (eg. using the prime numbers 961,748,941 and 982,451,653, found
; courtesy of a quick visit to Chris K. Caldwell's site at
;  https://primes.utm.edu/lists/small/millions/

;   n        |  Time (+ test-divisor 1)  |  Time (next test-divisor)  |  Ratio 
; ------------------------------------------------------------------------------
; 961748941  | 22741                     | 15359                      | 1.481
; 982451653  | 24298                     | 17356                      | 1.400


; Here, the numbers are bigger, and we can see that the ratios are more
; consistent: putting the modified procedure at about 1.4 times faster than
; the non-modified one. Here there are that many more numbers, that cutting
; out half of them -- even though we do more work with the ones that we do
; end up looking at -- is an efficient choice.