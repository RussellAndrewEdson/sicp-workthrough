;;; SICP Exercise 1.11
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us writing procedures for the function f, which is
;;; defined as follows:
;;;
;;;    f(n) = n,                              if n < 3
;;;         = f(n-1) + 2*f(n-2) + 3*f(n-3),   if n >= 3.
;;;
;;; We wanted two procedures: one that computes f using a recursive process,
;;; and one that uses an iterative process.


; The recursive process procedure is straight-forward.
; (I've named it f1 here to distinguish it from the iterative one.)

(define (f1 n)
  (cond ((< n 3) n)
        (else (+ (f1 (- n 1))
                 (* 2 (f1 (- n 2)))
                 (* 3 (f1 (- n 3)))))))

; We can test this with some simple values to see that it works as expected.
; In particular, it should at least work for n = 0,1,2:

(f1 0)
;> 0

(f1 1)
;> 1

(f1 2)
;> 2

; And it ought to work for higher values of n too.

; We should get f(3) = f(2) + 2*f(1) + 3*f(0) = 2 + 2(1) + 3(0) = 4 here:
(f1 3)
;> 4

; Then f(4) = f(3) + 2*f(2) + 3*f(1) = 4 + 2(2) + 3(1) = 11 :
(f1 4)
;> 11

; ... And so on.

(f1 15)
;> 142717


; For the procedure to generate the iterative process, we can note that for
; any n < 3, the procedure simply returns n. So we don't need to worry about
; negative numbers, and we also get f(0)=0, f(1)=1 and f(2)=2 as starting 
; values.

; Then we can represent the computation as a series of transformations on
; three state variables a, b and c (initialised to the above starting values) 
; as follows:

; a_0 <- 0
; b_0 <- 1
; c_0 <- 2

; a_(n+1) <- b_n
; b_(n+1) <- c_n
; c_(n+1) <- c_n + 2*b_n + 3*a_n

; And at the final iteration, we want to return the value of c.

; So we can write the procedure to generate the iterative process:

(define (f2 n)
  (define (f2-iter a b c count)
    (if (= 0 count)
        c
        (f2-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
  (if (< n 3)
      n
      (f2-iter 0 1 2 (- n 2))))

; Note that we limit the count at n-2, to account for the fact that
; n=1 and n=2 do not require iteration to determine. (That is, iteration
; starts from n=3, so to find f(n) we only need to iterate n-2 times.)

; Further, note that the (n < 3) check is in f2 proper, and not the iteration
; procedure. This stops us from having to make that check every time.

; We can test this f2 procedure too:

(f2 0)
;> 0

(f2 1)
;> 1

(f2 2)
;> 2

(f2 3)
;> 4

(f2 4)
;> 11

(f2 15)
;> 142717

; We see that we get the same answers as for the recursive process 
; (and much quicker, too.)