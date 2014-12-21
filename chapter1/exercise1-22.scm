;;; SICP Exercise 1.22
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise gave us a timed-prime-test procedure that would
;;; use the (runtime) primitive to report the amount of time it took
;;; to determine if a given number was prime. Using the procedure, we
;;; were to write our own procedure to check for primes in a specified
;;; range, and make notes on the run-time of the procedure.

; The timed-prime-test procedure (along with all of the ancillary
; procedures) is as follows:

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


; Now note that for searching for the primes in a given range, we
; only need to check the odd numbers (an even number is always divisible
; by 2, of course.)
;
; Then our search-for-primes procedure could be written as follows:

(define (search-for-primes start end)
  (define (search-iter n)
    (timed-prime-test n)
    (if (> n end) 
        (display "\nFinished searching for primes.")
        (search-iter (+ n 2))))
  (if (odd? start)
      (search-iter start)
      (search-iter (+ start 1))))


; With this procedure, we can find the three smallest primes above
; 1000 (I've truncated the output to only show the three primes found,
; but it actually reports every odd number in the range.)
; Recall that the primes are denoted with three asterisks (***).

(search-for-primes 1000 1200)
;> 1001
;> 1003
;> 1005
;> 1007
;> 1009 *** 22
;> 1011
;> 1013 *** 22
;> 1015
;> 1017
;> 1019 *** 26
;> ...
;> ...
;> Finished searching for primes.


; As well as the three smallest primes larger than 10,000:

(search-for-primes 10000 10200)
;> 10001
;> 10003
;> 10005
;> 10007 *** 108
;> 10009 *** 108
;> 10011
;> 10013
;> 10015
;> 10017
;> 10019
;> 10021
;> 10023
;> 10025
;> 10027
;> 10029
;> 10031
;> 10033
;> 10035
;> 10037 *** 107
;> ...
;> ...
;> Finished searching for primes.


; The 3 smallest primes larger than 100,000:

(search-for-primes 100000 100200)
;> 100001
;> 100003 *** 274
;> 100005
;> 100007
;> 100009
;> 100011
;> 100013
;> 100015
;> 100017
;> 100019 *** 340
;> 100021
;> 100023
;> 100025
;> 100027
;> 100029
;> 100031
;> 100033
;> 100035
;> 100037
;> 100039
;> 100041
;> 100043 *** 197
;> ...
;> ...
;> Finished searching for primes.


; And finally, the 3 smallest primes larger than 1,000,000:

(search-for-primes 1000000 1000200)
;> 1000001
;> 1000003 *** 1040
;> 1000005
;> 1000007
;> 1000009
;> 1000011
;> 1000013
;> 1000015
;> 1000017
;> 1000019
;> 1000021
;> 1000023
;> 1000025
;> 1000027
;> 1000029
;> 1000031
;> 1000033 *** 1473
;> 1000035
;> 1000037 *** 619
;> ...
;> ...
;> Finished searching for primes.


; Now we're given that the testing algorithm has order of growth 
; Theta(sqrt(n)), so we expect that testing for primes around 10,000
; should take sqrt(10) ~ 3.16 times as long as testing for primes around
; 1000. If we check our numbers from above, this seems to be about right:

; Primes around 1000:
;   1009 *** 22
;   1013 *** 22
;   1019 *** 26

; Primes around 10,000:
;   10007 *** 108
;   10009 *** 108
;   10037 *** 107


; And the primes around 100,000 should take about sqrt(100) = 10 times as
; long as those for 1000:

; Primes around 1000:
;   1009 *** 22
;   1013 *** 22
;   1019 *** 26

; Primes around 100,000:
;   100003 *** 274
;   100019 *** 340
;   100043 *** 197

; So the numbers are pretty much what we'd expect.


; And the time to find the primes around 1,000,000 should also be
; about sqrt(100) = 10 times as fast as finding primes around 10,000:

; Primes around 10,000:
;   10007 *** 108
;   10009 *** 108
;   10037 *** 107

; Primes around 1,000,000:
;   1000003 *** 1040
;   1000033 *** 1473
;   1000037 *** 619

; Once again, the numbers line up with what we'd want.


; So for the most part, the notion that programs on a machine run
; in time proportional to the number of steps required for the
; computation is supported by the data. There are some anomalies though:
; note that the times to find the primes 100043 and 1000037 are quite
; shorter than the times taken for the primes in their respective ranges.