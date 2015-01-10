;;; SICP Exercise 1.41
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise we wrote a procedure 'double' that returns a 
;;; procedure that applies the original procedure twice (ie. composes it
;;; with itself.)


; We're given that the procedure to apply will only ever take one argument,
; so we can write our double procedure simply as follows:

(define (double f)
  (lambda (x)
    (f (f x))))

; We can test this with an increment procedure (applying it twice should +2
; to the given argument.

(define (inc x)
  (+ x 1))

((double inc) 3)
;> 5

((double inc) 7)
;> 9


; Now what is the value of the expression (((double (double double)) inc) 5) ?
; We can use the substitution model to figure this out.

; To keep things from getting too unwieldy, we'll figure out what that 
; (double double) expression evaluates to first, using the fact that we can
; represent double as (lambda (f) (lambda (x) (f (f x)))) :

(double double)




; Now we can apply the double procedure to this:

(double (double double))




; Now we apply that whole thing to the inc function, and it should
; wind up quite nicely.

((double (double double)) inc)



    
     

