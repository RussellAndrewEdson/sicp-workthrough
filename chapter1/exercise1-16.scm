;;; SICP Exercise 1.16
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we needed to define a procedure to make an iterative
;;; exponentiation process similar to the fast-expt procedure detailed in 
;;; the book.
;;;
;;; We were given the hint that (b^(n/2))^2 = (b^2)^(n/2), and that we should
;;; aim to keep an additional state variable a that takes the initial value
;;; 1 and contains the desired result at the end, and define the state 
;;; transformation so that the product a*b^n is an invariant quantity.

; The fast-expt procedure from the book was defined more or less as follows
; (I've changed the name of the procedure to stop a clash later on):

(define (fast-expt-recur b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-recur b (/ n 2))))
        (else (* b (fast-expt-recur b (- n 1))))))


; So keeping the hint in mind, we can note the following:
;   1. When n isn't even, we still do the normal thing in the iterative
;      procedure. That is, we decrement n and multiply another b onto the
;      running product:
;        b <- b
;        n <- n-1
;        a <- b*a
;
;      Note that our invariant still holds. ab^n = (b*a)*b^(n-1) is true.
;
;   2. The state variable a carries the result at the end of the process, 
;      so once we no longer do any more multiplications (ie. n = 0), we
;      simply return a.
;
;   3. When n is even, then the hint gives us that b^n = (b^2)^(n/2). So
;      we make the following transformations:
;        b <- b^2
;        n <- n/2
;        a <- a.
;
;      Then note that ab^n = a*(b^2)^(n/2), so we maintain our invariant.


; So our procedure is then as follows:

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (* b b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* a b))))) 
  (fast-expt-iter b n 1))


; We can see with a few examples that these two procedures seem to agree
; on their values.

(fast-expt-recur 0 2)
;> 0
(fast-expt 0 2)
;> 0

(fast-expt-recur 1 0)
;> 1
(fast-expt 1 0)
;> 1

(fast-expt-recur 2 8)
;> 256
(fast-expt 2 8)
;> 256

(fast-expt-recur 3 123)
;> 48519278097689642681155855396759336072749841943521979872827
(fast-expt 3 123)
;> 48519278097689642681155855396759336072749841943521979872827

