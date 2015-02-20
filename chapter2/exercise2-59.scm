;;; SICP Exercise 2.59
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we implemented the 'union-set' operation for the set
;;; representation as an unordered-list.


; We'll need our 'element-of-set?' procedure as defined in the book:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


; Now we can find the union of two sets set1 and set2 in a recursive way
; with the following algorithm (similar to the definition for 'append'):
;   - If set1 is nil, we'll return set2.
;   - Otherwise, we'll check to see whether (car set1) is in set2.
;     - If it is, we'll return the union of the cdr of set1 with set2.
;     - If not, we'll cons the car of set1 onto the front of the list
;       we get from the union of the cdr of set1 with set2.

; That is:

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))


; We can test this procedure with some examples.

(union-set '(1 7 2 3) '(1 5 9 8 3))
;> (7 2 1 5 9 8 3)

(union-set '(1 2) '())
;> (1 2)

(union-set '() '())
;> ()