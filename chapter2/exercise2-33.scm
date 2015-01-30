;;; SICP Exercise 2.33
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we simply filled in the blanks to complete
;;; the given definitions for some basic list operations written in
;;; terms of accumulations.


; For testing purposes, we'll bring in the accumulate procedure 
; given in the book:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; The thing to note about the procedure here is that it will first
; recursively call itself until we reach the end of the sequence
; (ie. using the substitution model, the call to the op procedure
; needs to determine its arguments...). Then it will work its way
; back up, doing the accumulation as it goes. 
;
; eg. If we said (accumulate + 0 '(1 2 3)), our order of operations
; would in fact be 0 + 3 + 2 + 1 = 6.
;
; This is just something to keep in mind (if we were defining an
; iterative process for the accumulation, we would probably write it
; the other way around.)


; So for our map procedure, we'd end up at the last element in the
; list, which we want to apply the procedure p to, and then cons 
; with the empty list. On the next call up the stack, we'd cons the
; result of applying p to -that- element to the growing list, and
; so on until we reach the top again. So at each point we're consing
; the value of (p x) to the rest of our new list y.
;
; Hence we can define our 'map' procedure as follows (I've called
; it map2 to prevent any confusion with the existing map primitive):

(define (map2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

; We can test the procedure with the usual square procedure as
; follows:

(define (square x)
  (* x x))

(map2 square (list 1 2 3 4 5))
;> (1 4 9 16 25)


; Now for append, we simply need to fill in our initial value and
; the sequence to accumulate.

; Recall that our previous definition for append was this:
;;  (define (append list1 list2)
;;    (if (null? list1)
;;      list2
;;      (cons (car list1) (append (cdr list1) list2))))

; So using the same idea, we want to start with the initial value
; of our second sequence seq2, and recursse down to the last
; element of seq1. Then as we move back up seq1, we want to cons
; each element to the front of our growing list at each point.

; So this procedure is actually quite simple to implement:

(define (append2 seq1 seq2)
  (accumulate cons seq2 seq1))

; We ought to test this, as always:

(append2 (list 1 5 2 3) (list 5 7 6))
;> (1 5 2 3 5 7 6)


; Finally, for our length procedure, we simply want to increment
; a running total as we move back up the sequence. So we can really 
; use a sort of 'inc' procedure here, where we take in the current
; element x and the accumulated total y, and simply add 1 to y
; (so we don't do anything with the argument x):

(define (length2 sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; We can test this one too:

(length2 (list 3 2 3 1 54 2 1 3))
;> 8