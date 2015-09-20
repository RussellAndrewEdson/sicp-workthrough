;;; SICP Exercise 3.8
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we design a procedure (using assignment) that
;;; is fully dependent on the order of evaluation of its arguments.


; We want to design a procedure 'f' such that evaluating:
;   (+ (f 0) (f 1))
; 
; will return 0 if the arguments are evaluated left-to-right, and
; returns 1 if the arguments are evaluated right-to-left.


; One easy idea is this. Suppose we define the function f to return
; its argument the first time it is ever called, but then return 0
; every subsequent time, regardless of what its argument is.
; 
; That is, suppose we have a procedure that looks like this:

(define (instantiate-f)
  (let ((called-previously false))
    (lambda (x)
      (if (not called-previously)
          (begin
            (set! called-previously true)
            x)
          0))))


; Then we can construct our 'f' procedure as follows:

(define f (instantiate-f))

; And this procedure will work as required!


; We can demonstrate that this works, too. Let 'f-left' be
; such a procedure, for instance:

(define f-left (instantiate-f))

; Then evaluating the expression (+ (f-left 0) (f-left 1)) from left 
; to right would give us the following sequence of argument evaluations
; (note that we're using the let* form here, since we're being strict
; about the evaluation order.)

(let* ((first-evaluation (f-left 0))
       (second-evaluation (f-left 1)))
  (+ first-evaluation second-evaluation))
;> 0


; On the other hand, if we evaluate from right-to-left (with our
; procedure 'f-right'), we get 1 instead of 0:

(define f-right (instantiate-f))

(let* ((first-evaluation (f-right 1))
       (second-evaluation (f-right 0)))
  (+ first-evaluation second-evaluation))
;> 1