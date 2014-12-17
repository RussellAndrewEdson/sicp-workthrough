;;; SICP Exercise 1.18
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to combine what we learned in exercises
;;; 1.16 and 1.17 to create a procedure that generates an iterative process
;;; for multiplying two integers that uses a logarithmic number of steps. 
;;;
;;; (So basically our procedure is to the fast-mult procedure of
;;; exercise 1.17, what our exercise 1.16 procedure was to fast-expt.)

; The fast-mult procedure from 1.17, along with the double and halve
; procedures, are copied below. (The name of the procedure has been
; changed though to prevent a clash when we name our new procedure
; fast-mult.)

(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (fast-mult-recur a b)
  (cond ((= 0 b) 0)
        ((even? b) (fast-mult-recur (double a) (halve b)))
        (else (+ a (fast-mult-recur a (- b 1))))))


; So the previous procedure generates a recursive process; we want a 
; procedure that generates an iterative one. Similar to what we did
; in exercise 1.16, define the state variable c as an invariant quantity, 
; and we want to construct the state transformations such that the sum
; c+(a*b) is preserved from state to state. c will initially be 0, and
; will hold the full result by the time the process finishes.

; So the state transformations are as follows.
;   When b isn't even, we do the normal thing. We decrement b, and
;   'pull out' an a to store into c:
;     a <- a
;     b <- b-1
;     c <- c+a
;
;   And when b is even, then we use the double/halve trick
;   (c doesn't change here.)
;     a <- 2*a
;     b <- (1/2)*b
;     c <- c
;
;   When b is 0, we end the process and return c.


; So putting all of that into an actual procedure, we have:

(define (fast-mult a b)
  (define (fast-mult-iter a b c)
    (cond ((= b 0) c)
          ((even? b) (fast-mult-iter (double a) (halve b) c))
          (else (fast-mult-iter a (- b 1) (+ c a)))))
  (fast-mult-iter a b 0))


; As always, we can run a couple of examples, and see that the answer
; appears to be correct for this new iterative process.

(fast-mult-recur 12 0)
;> 0
(fast-mult 12 0)
;> 0
(fast-mult-recur 0 12)
;> 0
(fast-mult 0 12)
;> 0

(fast-mult-recur 4352 391)
;> 1701632
(fast-mult 4352 391)
;> 1701632
(fast-mult-recur 391 4352)
;> 1701632
(fast-mult 391 4352)
;> 1701632