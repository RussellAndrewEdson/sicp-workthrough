;;; SICP Exercise 2.61
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we gave an implementation of the adjoin-set
;;; operation in terms of the ordered set representation.


; Since we've got an ordered set representation now, we could write
; the element-of-set? procedure in the following way, as done in the
; book:

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

; Note here that we're taking advantage of the ordering of the list by
; immediately returning false if we ever come across an element that is 
; larger than the one we're looking for.


; Now for our adjoin-set procedure, we can use a similar idea! We want to
; place the element in its correct spot in the ordered list, so as we cdr
; down the list, we'll compare the elements. If we come across one that is
; equal to the element we're appending, we don't do anything. However, if
; we come across an element that is -larger-, we know we're done: we insert
; the element at that position and return.

; Now we need to figure out how this insertion will work. While it's true that
; we'll be consing the element at the position where a bigger element exists,
; we still need to consider the elements we've already seen as we cdr-ed up the
; list! So in our 'else' clause, we'll actually want to be "rebuilding" the
; list as we go up.

; So our algorithm is as follows:
;   - If we have the empty list/set, we'll cons x to the front and return.
;   - If we have the case where x is equal to the car of the set, we won't do
;     anything, and we'll just return the set as is.
;   - If we have the case where x is smaller than the car of the set, we need
;     to cons x in front of the set and return this.
;   - Otherwise, we want to cons the car of the set to the result of 
;     adjoining the element x to the cdr of the set.

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

; On average, this procedure here should only have to check about n/2 elements
; before we insert the element at its right spot -- so we require about half
; as many steps compared to the unordered representation.


; Of course, we should test this to make sure that it works as intended:

(adjoin-set 6 '(1 3 4))
;> (1 3 4 6)

(adjoin-set 10 '(1 2 4 7 8 9 11))
;> (1 2 4 7 8 9 10 11)

(adjoin-set 5 '(1 2 3 4 5 6 7))
;> (1 2 3 4 5 6 7)

(adjoin-set 1 '())
;> (1)

(adjoin-set 2 '(1))
;> (1 2)

(adjoin-set 1 '(2 4 5 6 7))
;> (1 2 4 5 6 7)