;;; SICP Exercise 1.4
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise was to apply the evalation model to the given procedure
;;; to determine its behaviour.

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; During evaluation, the operator (if (> b 0) + -) is evaluated to give 
; us either + or - depending on whether b is positive or not. Then the
; operands a and b are evaluated to get their values, and the + (or -)
; procedure is applied to those values to get the value of the combination.

(a-plus-abs-b 1 -2)
;> 3

(a-plus-abs-b -2 0)
;> -2
; When b=0, we are simply subtracting 0, so this still works.
