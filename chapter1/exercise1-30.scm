;;; SICP Exercise 1.30
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we rewrote the 'sum' procedure so that it
;;; generates an iterative process.


; The recursive process version is included below:

(define (recursive-sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (recursive-sum term (next a) next b))))


; Rewriting this as a procedure that generates an iterative process
; instead is pretty straight-forward by now:

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (+ result (term a)))))
  (iter a 0))


; A quick example shows that it seems to work the same as the
; recursive one.

(define (inc x)
  (+ x 1))

(sum identity 1 inc 100)
;> 5050


; Curiously, if we are dealing with very small numbers (for example),
; we actually get slightly different answers between them.
; Consider the following example, where we have a partial sum of the
; harmonic series 1/n:

(define (one-over-n n) 
  (/ 1.0 n))

(sum one-over-n 1.0 inc 10000.0)
;> 9.787606036044348

(recursive-sum one-over-n 1.0 inc 10000.0)
;> 9.787606036044386

; Of course, this is probably due to the differences in how we're summing
; the numbers up. The iterative process has us accumulating the result as
; we associate from left to right (so the largest numbers are added first),
; whereas the recursive process has delayed addition operations, so we end 
; up associating from right to left and summing the smallest numbers first.
; Such a difference is bound to have an effect when we're dealing with
; the boundaries of our representation.