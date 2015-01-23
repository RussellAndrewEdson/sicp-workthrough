;;; SICP Exercise 2.17
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us defining a procedure last-pair that would
;;; return the list that contains only the last element of a 
;;; non-empty list (ie. the last cons cell, containing the last 
;;; element and the empty list.)


; Now as hinted above, the last cons cell of a list will be the one
; that has the empty list as its cdr. So we simply cdr down the list,
; checking for the empty list as we go. 
;
; Note though that we returns the cell itself, and not the car of the
; cell! (Since we return the -list- containing the last element...)

(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))


; We can test this out with the given example:

(last-pair (list 23 72 149 34))
;> {mcons 34 '()}