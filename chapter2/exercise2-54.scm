;;; SICP Exercise 2.54
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the 'equal?' procedure, which
;;; returns true if the two given lists contain equal elements arranged
;;; in the same order.


; We can implement this recursively using the eq? procedure with a cond
; statement. We first check whether we have a list structure in both 
; arguments: if we do, we check for equality by cdring down the lists as
; usual. 
;
; If not though, we'll check to see whether the two arguments are
; equal to each other with eq? If they are, then we'll return true.
; For any other outcome, we'll return false.

; (For readability, we'll define a separate procedure, 'both-pairs?', that
; returns true if the given arguments both have list structure.)

(define (both-pairs? a b)
  (and (pair? a) (pair? b)))

(define (equal? a b)
  (cond ((both-pairs? a b) (and (equal? (car a) (car b))
                                (equal? (cdr a) (cdr b))))
        ((eq? a b) true)
        (else false)))


; We can test this procedure to see if it works correctly:

(equal? '(this is a list) '(this is a list))
;> #t

(equal? '(this is a list) '(this (is a) list))
;> #f

(equal? '(this is a list) '(a list is this))
;> #f

(equal? 'a 'a)
;> #t

(equal? '() '())
;> #t

; Note that our 'equal?' procedure also appears to work with numbers,
; though this is a Scheme implementation-specific behaviour. So if we
; really wanted to be rigorous, we should test for numbers separately,
; as suggested in footnote 36.

(equal? 3 3)
;> #t 
; (...But not necessarily!)