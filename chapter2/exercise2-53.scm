;;; SICP Exercise 2.53
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we predicted what the interpreter would
;;; print in response to the given expressions.


; First off, we have the definition of the memq procedure as 
; given in the book (I've commented it out since it is already 
; available in the Racket SICP package, but we'll include the 
; book's definition for reference.)

;;  (define (memq item x)
;;    (cond ((null? x) false)
;;          ((eq? item (car x)) x)
;;          (else (memq item (cdr x)))))


; Now our first expression is (list 'a 'b 'c).
; The quote (') marks tell the interpreter to read a, b and c as
; symbols, instead of evaluating them. Then we're simply creating
; a list of those symbols.
;  ie. We'll get the list (a b c) from the interpreter.

; Sure enough:

(display (list 'a 'b 'c))
;> (a b c)


; For the expression (list (list 'george)), We've got the symbol
; 'george inside a doubly-nested list, ie. ((george)).

; Which is exactly what we get when the expression is evaluated:

(display (list (list 'george)))
;> ((george))


; For (cdr '((x1 x2) (y1 y2))), we quote the list ((x1 x2) (y1 y2)), and
; then retrieve the cdr of that list, which will be the list ((y1 y2)).

(display (cdr '((x1 x2) (y1 y2))))
;> ((y1 y2))


; Now we have (cadr '((x1 x2) (y1 y2))). From the previous expression, we
; know that the cdr of that quoted list is ((y1 y2)). So when we take the
; car of this, we'll get the list (y1 y2), without the double-nesting.

(display (cadr '((x1 x2) (y1 y2))))
;> (y1 y2)


; When (pair? (car '(a short list))) is evaluated, we return the car of that
; quoted list, which is the symbol a, and test to see whether it is a pair.
; Since symbols aren't pairs, we expect to see the check return false.

(display (pair? (car '(a short list))))
;> #f


; Now we have the first memq expression, 
;   (memq 'red '((red shoes) (blue socks)))
; As you can see from the definition for memq above, we only move through
; the top level of the list in our check for equality. So the symbol red is
; -not- actually equal to the -list- containing the symbols red and shoes,
; so the expression evaluates to false.

(display (memq 'red '((red shoes) (blue socks))))
;> #f


; Finally, we have the expression
;   (memq 'red '(red shoes blue socks))
; This time, we do have red appearing as a symbol in that (top-level) list,
; so we are returned the list containing all of the elements from the symbol
; red onwards. Which turns out to be the entire list:

(display (memq 'red '(red shoes blue socks)))
;> (red shoes blue socks)
