;;; SICP Exercise 1.1 
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise was mainly just anticipating the results of 
;;; expressions run through the interpreter (and then running them).
;;; After each expression, the output from the interpreter appears
;;; in a comment: ;> . Any additional comments are added below.

10  
;> 10

(+ 5 3 4)
;> 12

(- 9 1)
;> 8

(/ 6 2)
;> 3

(+ (* 2 4) (- 4 6))
;> 6

(define a 3)
;>
; Note that the interpreter doesn't return anything for a variable binding!

(define b (+ a 1))
;>
; Again, we still don't get displayed output for a binding.

(+ a b (* a b))
;> 19

(= a b)
;> #f
; ie. false.

(if (and (> b a) (< b (* a b)))
    b
    a)
;> 4
; We return the value of b, since the predicate evaluates to true.

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;> 16
; As b=4, the second clause is true.

(+ 2 (if (> b a) b a))
;> 6
; The predicate is true, so we add 2 and b together.

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;> 16
; The second clause is true, so we multiply b and a+1.
