;;; SICP Exercise 1.24
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we want to modify the timed-prime-test procedure
;;; that we had back in Exercise 1.22 so that it uses the fast-prime? Fermat
;;; method, and compare the run-time for the primes that we found earlier.

; The timed-prime-test and fast-prime? procedures, along with the modification
; (and all of the ancillary procedures), are defined below.
; For the Fermat method, we'll test only once for each candidate prime.
; Obviously testing each prime multiple times would impact the running time.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

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


; We'll test each of the primes found in Exercise 1.22, using the same
; tabulated data that we had in Exercise 1.23. The results have been
; recorded in the table below (once again, the procedures for each prime
; were called in separate new environments so that nothing that Scheme is 
; doing in the background to speed things up (or slow them down) will  
; impact on the results between the tests.)

;   n        |  Time (prime?)  |  Time (fast-prime?)  |  Ratio
; ----------------------------------------------------------------
; 1009       | 157             | 246                  | 0.638
; 1013       | 181             | 248                  | 0.730
; 1019       | 257             | 309                  | 0.832
; 10007      | 457             | 469                  | 0.974
; 10009      | 326             | 314                  | 1.038
; 10037      | 226             | 385                  | 0.587
; 100003     | 355             | 347                  | 1.023
; 100019     | 354             | 489                  | 0.724
; 100043     | 341             | 381                  | 0.895
; 1000003    | 952             | 352                  | 2.705
; 1000033    | 764             | 318                  | 2.403
; 1000037    | 935             | 288                  | 3.247


; Now since the Fermat test has Theta(log n) growth, we expect that
; the time to test primes near 1,000,000 should be twice as long as
; that time taken to test primes near 1000:
;
; log(1000000) / log(1000) = 2.

; Though this doesn't appear to be the case in the data, where there
; is a striking similarity between the amounts of times across the board.
; Even numbers near 1,000,000 seem to be found in roughly the same amount
; of time:

(timed-prime-test 1000000033)
;> 1000000033 *** 292

(timed-prime-test 1000000663)
;> 1000000663 *** 434


; This discrepency could be explained by the fact that the method is fast
; enough that pretty much all of the time spent in the procedure is
; just overhead at this point (the other procedure calls, finding
; the random number, etc.) and we'd need much larger numbers to be able
; to see the size of the input (n) come into play with respect to the
; running time for the procedure.
;
; (Alternatively, increasing the number of tests for each prime candidate
; would ramp up the amount of time taken by the procedure too, of course.)