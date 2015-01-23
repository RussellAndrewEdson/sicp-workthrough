;;; SICP Exercise 2.20
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we use the dotted-tail notation for arbitrary
;;; numbers of parameters to create a procedure 'same-parity' that 
;;; takes one or more integers and returns the list of all the 
;;; arguments that have the same even-odd parity as the first argument.


; A few things to note first. We guaranteed that the return list will 
; contain at least the first argument to the procedure, so we can simply
; store that in a list and append on any arguments that we find with the
; same parity.

; Second, we can use a closure to define a 'has-same-parity?' check that
; compares a given argument with the first argument.

; What we basically want to be doing is cdring down the list as always, 
; and constructing the result list as we go. As we saw in Exercise 2.18,
; this sort of thing is easily done with an iterative process.

; So our same-parity procedure is as follows:

(define (same-parity first . rest)
  (define (has-same-parity? arg)
    (or (and (even? first) (even? arg))
        (and (odd? first) (odd? arg))))
  (define (iter items same-parity-list)
    (if (null? items)
        same-parity-list
        (iter (cdr items)
              (if (has-same-parity? (car items))
                  (append same-parity-list (list (car items)))
                  same-parity-list))))
  (iter rest (list first)))


; Let's try it out on the given examples to make sure it works:

(same-parity 1 2 3 4 5 6 7)
;> {mcons 1 {mcons 3 {mcons 5 {mcons 7 '()}}}}

(same-parity 2 3 4 5 6 7)
;> {mcons 2 {mcons 4 {mcons 6 '()}}}