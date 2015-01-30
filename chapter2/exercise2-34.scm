;;; SICP Exercise 2.34
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us looking at Horner's rule for polynomial
;;; evaluation, and filling in a template to represent it in terms
;;; of an accumulation.

; So we have our accumulate procedure here:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; Now we're given the initial value of 0 and a sequence for the
; coefficients of the polynomial as arguments. Our job is to fill
; in the blanks on the op procedure, which should take two arguments,
; this-coeff and higher-terms.

; Now keep in mind how the accumulate procedure works! Assuming that
; our sequence looks like a_1, a_2, ... , a_n, we've recursed all the
; way down to a_n before we've done anything, and we've got 0 for our
; running result.
; So what we want to do is:
;  - multiply our running result (which is 0 at the moment) by x,
;  - add the current coefficient (this-coeff = a_n).

; Then as we move back up the chain, we'll multiply the total result
; by x and add on the value of this-coeff each time. This will get us
; what we want.

; So our Horner's rule procedure is implemented as follows:

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ (* x higher-terms)
                                                   this-coeff))
              0
              coefficient-sequence))


; We can then test this with the given example. At x=2, the polynomial
; 1 + 3x + 5x^3 + x^5 should return 1 + 6 + 40 + 32 = 79:

(horner-eval 2 (list 1 3 0 5 0 1))
;> 79