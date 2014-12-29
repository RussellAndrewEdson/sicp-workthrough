;;; SICP Exercise 1.34
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we investigated the effect of calling a
;;; procedure with itself as an argument.

; We have the following procedure:

(define (f g)
  (g 2))


; The procedure works as follows:

(define (square x)
  (* x x))

(f square)
;> 4

(f (lambda (z) (* z (+ z 1))))
;> 6


; Suppose we try to evaluate the expression (f f). What happens?

;;  (f f)

; Running that code gives us the following error:
;> procedure application: expected procedure, given: 2; arguments were: 2


; That error message seems a little cryptic right now, so let's use the 
; substitution model to see what's really going on here. Recall that 
; we evaluate the arguments, then evaluate the operator and apply. 
; Note also that we can represent the function f as (lambda (g) (g 2)).

;;  (f f)
;;  (f (lambda (g) (g 2)))
;;  ((lambda (g) (g 2)) (lambda (g) (g 2)))
;;  ((lambda (g) (g 2)) 2)
;;  (2 2)

; Step by step, we evaluate the argument, which is the procedure f.
; The operator is also the procedure f (which are both represented in
; lambda form.) We then apply that procedure: the procedure takes the
; argument (the same procedure) and applies it to the value 2. The result
; of this is the somewhat weird-looking expression (2 2), which then needs
; to be evaluated.


; So we see why we got the error above. Once everything is fully evaluated,
; we're trying to apply the 'procedure' 2 to the value 2, which doesn't
; make any sense. Re-reading the interpreter's error message, we see that
; this is exactly what it was complaining about.