;;; SICP Exercise 1.28
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to implement the Miller-Rabin test
;;; for primality by slightly modifying the code we already have for
;;; the Fermat test.

; For our expmod procedure, we now want it to signal (by returning 0)
; whenever it discovers a nontrivial square root of 1 modulo n
; (ie. a number not equal to 1 or n-1 that squares to 1 mod n).
; We can easily write a procedure to make that check, as follows:

(define (signal-nontrivial-root num n)
  (if (and (not (= num 1))
           (not (= num (- n 1)))
           (= (remainder (square num) n) 1))
      0
      num))

(define (square x)
  (* x x))

; So our procedure here will return 0 if we have a nontrivial root,
; and return the normal number otherwise -- this allows us to
; embed the method into the existing expmod procedure.
;
; (In fact, you could literally embed the procedure into expmod as
; a block structure closure, which would allow you to remove the n
; parameter completely and just use expmod's m parameter instead. 
; But this exercise is complicated enough as it is, so I'm keeping 
; them separate for clarity.)


; Then we modify the expmod procedure by simply having it check
; for a nontrivial root in the squaring step. If it finds one, it 
; returns 0, which effectively cancels out the rest of the calculation,
; (anything multiplied by 0 is still 0, of course!). 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square 
                      (signal-nontrivial-root (expmod base (/ exp 2) m) m))
                      m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x)
  (* x x))


; So now our expmod procedure will return 0 from the whole process
; if it discovers a nontrivial square root of 1. By the Miller-Rabin
; test the number thus cannot be prime, so zeroing it out prevents
; any chance of a false positive for such a number when we check that 
; a^(n-1) is congruent to 1 mod n in the actual primality test.

; Then the test is as follows:

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))


; Now we should try the procedure on some known prime and non-prime numbers,
; and we ought to definitely show that the Carmichael numbers (from 
; Exercise 1.27) don't fool this test. So similar to what we had in that 
; exercise, we'll make a procedure that runs the Miller-Rabin test for 
; every number between 1 and n:

(define (passes-miller-rabin-test? n)
  (define (miller-rabin-test-iter a)
    (cond ((= a n) true)
          ((not (= (expmod a (- n 1) n) 1)) false)
          (else (miller-rabin-test-iter (+ a 1)))))
  (miller-rabin-test-iter 1))


; So testing the procedure on some small examples should
; return us the correct results:

(passes-miller-rabin-test? 5)
;> #t

(passes-miller-rabin-test? 13)
;> #t

(passes-miller-rabin-test? 19)
;> #t

(passes-miller-rabin-test? 8)
;> #f

(passes-miller-rabin-test? 12)
;> #f

(passes-miller-rabin-test? 26)
;> #f


; So far so good. Now for the Carmichael numbers:

(passes-miller-rabin-test? 561)
;> #f

(passes-miller-rabin-test? 1105)
;> #f

(passes-miller-rabin-test? 1729)
;> #f

(passes-miller-rabin-test? 2465)
;> #f

(passes-miller-rabin-test? 2821)
;> #f

(passes-miller-rabin-test? 6601)
;> #f


; So the Carmichael numbers from before do not fool the Miller-Rabin test.