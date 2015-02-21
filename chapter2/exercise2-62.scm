;;; SICP Exercise 2.62
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we gave a Theta(n) implementation of the union-set
;;; procedure for our ordered-list set representation.


; In the same way as in Exercise 2.61, we can take advantage of the ordering
; of the list to write a more efficient implementation for the union operation.

; Our algorithm is as follows (similar to the one for intersection-set given
; in the book:
;   - If either of the sets is null, we'll return the other set.
;   - Otherwise, we'll compare the first elements of the sets: x1 and x2.
;     - If they are equal, we append either element to the union of
;       the cdrs of both sets.
;     - If x1 < x2, then we cons x1 to the front of the union of the cdr
;       of set21 with set2.
;     - If x1 > x2, then we cons x2 to the front of the union of set1 with
;       the cdr of set2.

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else 
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 ((> x1 x2)
                  (cons x2 (union-set set1 (cdr set2)))))))))
           
; As with the intersection-set procedure, the number of steps for
; union-set is at most the sum of the sizes of set1 and set2, since 
; we're always cdring down at least one of them. So we have Theta(n) 
; growth.


; As always, we can test this procedure:

(union-set '(1 5 6 10) '(2 3 4 5))
;> (1 2 3 4 5 6 10)

(union-set '(1 2 3) '(4 5 6))
;> (1 2 3 4 5 6)

(union-set '(1 2) '())
;> (1 2)

(union-set '() '(3 4))
;> (3 4)

(union-set '() '())
;> ()

(union-set '(4 5 6 7) '(1 2 3 4 5))
;> (1 2 3 4 5 6 7)