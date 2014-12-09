;;; SICP Exercise 1.5
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us looking at a test to determine whether the
;;; interpreter is using applicative-order evaluation or normal-order
;;; evaluation.

; The following procedures are defined:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; Then the following expression is evaluated (I've commented it out for 
; reasons that will hopefully become apparent.)

;; (test 0 (p))


; We assume that the 'if' special form operates the same way regardless of
; the evaluation technique used.

; When applicative-order evaluation is used, the evaluation goes as follows:

; 1. The operator and operands are fully evaluated.
;    (I've used the lambda notation for the evaluations here; admittedly it
;    hasn't appeared in the book yet, but it describes what is happening well.)
;
;    Here test is evaluated to a procedure that takes in two arguments, and
;    applies that if structure defined above to them. 0 evaluates to 0, and (p)
;    evaluates to a procedure that takes no arguments and returns (p), ie. the
;    application of that procedure to no arguments.

;; (test 0 (p))
;; ( (lambda (x y) (if (= x 0) 0 y)) 0 (lambda () (p)) )

; 2. So test and 0 have been fully evaluated. But we're not done evaluating (p)!

;; (lambda () (p))
;; (lambda () (lambda () (p))) )
;; (lambda () (lambda () (lambda () (p)))) )
;; (lambda () (lambda () (lambda () (lambda () (p))))) )
;; ...
;; ...

; 3. Each time, (p) evaluates to a procedure that takes no arguments and
;    returns (p). But we need to evaluate that returned (p), so it gets
;    evaluated to a procedure that takes no arguments and returns (p),
;    which is evaluated to a procedure that takes in no arguments and 
;    returns (p), which is evaluated to a procedure ........

; So under applicative-order evaluation -- which you'll note is what the Scheme
; interpreter does -- this will never stop. (So you can see why I commented the
; expression out earlier!)


; On the other hand, when normal-order evaluation is used, it goes like this:

; 1. We substitute 0 and (p) in as parameters to the test procedure, without
;    evaluating them. The definition for test is fully expanded first.

;; (test 0 (p))
;; (if (= 0 0) 0 (p))

; 2. But the if special form doesn't evaluate the alternative if the predicate
;    is true, which it is (0 = 0). So we happily return 0.

; So basically the test will return 0 if normal-order evaluation is used, and the
; program will go into an infinite-loop if applicative-order evaluation is used.

; (There's a good explanation of this in Lecture 1 of the ArsDigita University
; Structure and Interpretation of Computer Programs course, available at:
;     http://aduni.org/courses/sicp/