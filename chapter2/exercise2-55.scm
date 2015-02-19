;;; SICP Exercise 2.55
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we investigated the interpreter's response to
;;; the expression (car ''abracadabra).


; As suggested, when we go to evaluate the expression (car ''abracadabra)
; at the interpreter, we magically get back 'quote'.

(display (car ''abracadabra))
;> quote

; The reason for this is simple: recall from footnote 34 that the quotation
; 'expr is essentially shorthand for the expression (quote expr).
; So our statement here is equivalent to (car '(quote abracadabra)), where
; the outer quotation mark means that we interpret "quote" as a symbol in this
; case, instead of as a special form.


; In fact, we can see this if we simply evaluate for the value of 
; ''abracadabra by itself:

''abracadabra
;> {mcons 'quote {mcons 'abracadabra '()}}

; So we can see that we have a list with the symbol quote as the first
; element, and the symbol abracadabra as the second element.
; In this form, it's obvious that taking the car of the list will return
; us the symbol quote.