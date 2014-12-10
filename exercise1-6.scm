;;; SICP Exercise 1.6
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we explore a new version of 'if' defined in terms
;;; of cond:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;;; This new if procedure is used to define a square-root procedure:

(define (sqrt-with-new-if x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; Ancillary procedures

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))


; However, when we try to run this new sqrt procedure, we end up
; in an infinite loop. The problem is that new-if is not a special
; form, so it has the same evaluation rules as normal. So for the
; expression:

;; (new-if (good-enough? guess x)
;;          guess
;;          (sqrt-iter (improve guess x)
;;                     x))

; The operator and all of the operands are evaluated fully, regardless
; of the truthity of the predicate. So to evaluate the expression, we need
; to completely evaluate that alternative call to sqrt-iter too, which will  
; itself have this new-if expression again -- which will call sqrt-iter again, 
; and so on, recursing forever (or until we run out of memory.)