;;; SICP Exercise 1.19
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us looking at an awesome algorithm for computing
;;; the Fibonacci numbers in a logarithmic number of steps. The algorithm
;;; depends on a clever generalisation of a state transformation that we'd
;;; previously seen in the book. The brunt of the algorithm was provided;
;;; we simply needed to fill in the appropriate expressions.

; For the fib-iter procedure from the book, we had the state transformation:
;    a <- a + b
;    b <- a

; Call this transformation T, then note that applying T n times to the pair
; (1,0) produces the pair (Fib(n+1), Fib(n)).

; That is, T^n (1,0) = (Fib(n+1), Fib(n)).


; Suppose we consider T to be a special case of a more general transformation
; T_pq, defined as follows:
;    a <- bq + aq + ap
;    b <- bp + aq
;
; Where taking p = 0, q = 1 gives us the transformation T.
;
; So writing this in a form more conducive to mathematical manipulation,
; we'll write T_pq (a,b) = (bq+aq+ap, bp+aq).

; Now note what happens when we apply T_pq twice (ie. consider the 
; transformation given by T_pq composed with itself):
;
; T_pq o T_pq (a,b) = T_pq (bq+aq+ap, bp+aq)
;                   = ( (bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p, (bp+aq)p + (bq+aq+ap)q )
;                   = ( bpq+aqq + bqq+aqq+apq + bqp+aqp+app, bpp+aqp + bqq+aqq+apq )
;                   = ( b(pq+qq+qp) + a(qq+pq+qp) + a(qq+pp), b(pp+qq) + a(qp+qq+pq) )
;                   = ( b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2), b(p^2 + q^2) + (q^2 + 2pq) )

; Now let p' = p^2 + q^2, and q' = q^2 + 2pq.
; Then we have that:
;
; T_pq o T_pq (a,b) = (bp'+aq'+ap', bp'+aq') 
;                   = T_p'q'.
;
; So we have the same form as for a single application, and so we have an
; explicit way to square the transformations, similar to what we had in
; the fast-expt procedure!


; So using this, we can fill in the blanks on the given definitions to
; create a procedure that runs in a logarithmic number of steps:
; (I've also added the usual square ancillary procedure)

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
