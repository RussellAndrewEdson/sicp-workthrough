;;; SICP Exercise 1.25
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we checked into whether we could've used our
;;; old fast-expt procedure in a call from the expmod one, saving us
;;; the work of basically rewriting the fast-expt logic within expmod.


; The original expmod procedure is as follows:

;;  (define (expmod base exp m)
;;    (cond ((= exp 0) 1)
;;          ((even? exp)
;;           (remainder (square (expmod base (/ exp 2) m))
;;                      m))
;;          (else
;;           (remainder (* base (expmod base (- exp 1) m))
;;                      m))))
;;
;;  (define (square x)
;;    (* x x))


; The new expmod procedure, using our old fast-expt procedure, is
; included below.

;;  (define (expmod base exp m)
;;    (remainder (fast-expt base exp) m))
;;
;;  (define (fast-expt b n)
;;    (cond ((= n 0) 1)
;;          ((even? n) (square (fast-expt b (/ n 2))))
;;          (else (* b (fast-expt b (- n 1))))))
;;
;;  (define (square x)
;;    (* x x))

; So could we have simply used this procedure instead, and saved ourselves
; some work? Let's investigate.


; For an example, suppose we want to evaluate (expmod 1234 6 7).
; Setting the substitution model to work on our original expmod procedure,
; we get the following:

;;  (expmod 1234 6 7)
;;  (remainder (square (expmod 1234 3 7)) 7)
;;  (remainder (square (remainder (* 1234 (expmod 1234 2 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 (remainder (square (expmod 1234 1 7)) 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 (remainder (square (remainder (* 1234 (expmod 1234 0 7)) 7)) 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 (remainder (square (remainder (* 1234 1) 7)) 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 (remainder (square (remainder 1234 7)) 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 (remainder (square 2) 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 (remainder 4 7)) 7)) 7)
;;  (remainder (square (remainder (* 1234 4) 7)) 7)
;;  (remainder (square (remainder 4936 7)) 7)
;;  (remainder (square 1) 7)
;;  (remainder 1 7)
;;  1

; Sure, there's quite a lot of delayed operations. But notice the numbers that
; we work with at each point. About the largest number that we ever see is
; 4936, which is relatively small in the grand scheme of things.
;
; In fact, the largest number we'll ever see is exactly 1234*6 = 7404, since 
; taking the remainder will always return a number smaller than 7. And note
; that we'll never be squaring anything larger than 6, too. So no matter how
; absurdly large the exponential calculation may be, we're always going to
; only be dealing with small numbers at each point here.
;
; (And notice that if at any point the result of a remainder expression is 0,
; then this carries right through and pretty much cancels out the rest of
; the computation (we'd then be squaring 0, multiplying 1234 by 0, or taking
; the remainder of 0 in every single expression after that!)


; Now suppose we use the fast-expt version. The substitution model gives us
; the following process:

;;  (expmod 1234 6 7)
;;  (remainder (fast-expt 1234 6) 7)
;;  (remainder (square (fast-expt 1234 3)) 7)
;;  (remainder (square (* 1234 (fast-expt 1234 2))) 7)
;;  (remainder (square (* 1234 (square (fast-expt 1234 1)))) 7)
;;  (remainder (square (* 1234 (square (* 1234 (fast-expt 1234 0))))) 7)
;;  (remainder (square (* 1234 (square (* 1234 1)))) 7)
;;  (remainder (square (* 1234 (square 1234))) 7)
;;  (remainder (square (* 1234 1522756)) 7)
;;  (remainder (square 1879080904) 7)
;;  (remainder 3530945043777457216 7)
;;  1

; Note that in the fast-expt version of the procedure, we can end up working
; with absolutely huge numbers! Depending on how big they get, that
; squaring operation (and then finding the remainder at the end) can be
; costly. And there's no chance of "cancelling out" the computation with a 0
; this time, either.
;
; (Also, not such a big deal in Scheme but worth mentioning: these numbers
; could get so big that we can't hold them in their designated memory spaces
; and they overflow. And we might not even be able to tell, since the
; remainder operation at the end would insidiously still return us a number
; between 0 and m. So the representation of integers in memory could end up
; being a concern here.)


; In regards to our fast prime tester procedure, note the way in which we
; call our expmod procedure in fermat-test:

;;  (define (fast-prime? n times)
;;    (cond ((= times 0) true)
;;          ((fermat-test n) (fast-prime? n (- times 1)))
;;          (else false)))
;;
;;  (define (fermat-test n)
;;    (define (try-it a)
;;      (= (expmod a n n) a))
;;    (try-it (+ 1 (random (- n 1)))))

; Here n can be huge, and a is a random number between 1 and n.
; With the new expmod procedure we'd almost certainly be dealing with
; ridiculously huge numbers, but with the original expmod the largest
; number we'd ever see in a calculation is (n-1)^2.
; (Since we can see at most a*(n-1) or (n-1)^2 as the result of any
; of the multiplying or squaring calculations respectively, and a can 
; be at most n-1 itself.)