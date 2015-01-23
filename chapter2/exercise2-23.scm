;;; SICP Exercise 2.23
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we give an implementation for the higher-order 
;;; procedure 'for-each', which operates similarly to map but simply
;;; applies the procedure to the elements and doesn't collect them
;;; (it is mainly used for side-effects, like printing.)

; We can define for-each in the following way (using true as the 
; arbitrary return value, as suggested):

(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items))
              (for-each proc (cdr items)))))

; We can test our procedure using the given example:

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
;> 57
;> 321
;> 88
