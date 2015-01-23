;;; SICP Exercise 2.18
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us constructing a procedure 'reverse' that took
;;; in a list and returned the list where all of the elements were in
;;; the reverse order.


; Now we can reverse a list by cdring down to the end of it, consing
; together the reverse list as we go. So we'll cdr down until we reach
; the end (when the cdr is the empty list), and at each point we'll
; cons the car of the current list onto our reverse list.

; In fact, this procedure is probably going to be easiest to write
; as an iterative process, where we can see the reverse list being
; built up in the iter procedure at each point:

(define (reverse items)
  (define (reverse-iter rest-of-items reverse-list)
    (if (null? rest-of-items)
        reverse-list
        (reverse-iter (cdr rest-of-items)
                      (cons (car rest-of-items) reverse-list))))
  (reverse-iter items nil))


; We can test it with the given example:

(reverse (list 1 4 9 16 25))
;> {mcons 25 {mcons 16 {mcons 9 {mcons 4 {mcons 1 '()}}}}}
      