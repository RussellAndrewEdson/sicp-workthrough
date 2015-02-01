;;; SICP Exercise 2.42
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we completed the definitions for the 
;;; 'queens' procedure that generates solutions for the n-queens
;;; puzzle.


; We have the 'queens' procedure as given in the book (together
; with the ancillary filter/flatmap/enumerate/etc procedures):

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; What we need to do is implement the procedure 'adjoin-position',
; the variable 'empty-board' and the predicate 'safe?'.

; Now from a read of the above code, it seems like we're representing
; queen positions using a (row, column) list. So in our adjoin-columns
; procedure, we should do just this, and then cons the new pair onto
; the existing rest-of-queens list:

(define (adjoin-position row column positions)
  (cons (list row column) positions))


; Next, we need an implementation for the empty-board definition.
; We'll be accessing this value when we recurse all the way down to
; k = 0, at which point we'll be consing on new (row,column) pairs
; as per our adjoin-position procedure. So we can simply define the
; empty-board to be the empty list (nil) here:

(define empty-board nil)


; Now for the 'safe?' procedure, we want to return true when the
; queen in column k is safe for a given set of positions. As given,
; we can assume that all of the queens in columns 1 to k-1 are okay,
; (by the recursion), so we simply need to check the newest queen
; position for the given board, which will be in column k.

; So from chess rules, a given queen can check pieces which are:
;   - in the same column (but we won't have to worry about this.)
;   - in the same row,
;   - exactly diagonal from the queen.

; So since we're only dealing with queens, we'll say that out newest
; queen is safe if she can't check any of the existing queens (ie.
; so vice-versa: the existing queens can't check her, either!)

; In fact, we can isolate these rules into new procedures that
; take two queen positions and return true if:

; 1. The queens are in the same row:

(define (same-row? q1 q2)
  (= (car q1) (car q2)))

; 2. The queens are diagonal from each other.
;    (We can write this as follows. Suppose we have queens in positions
;    (r1,c1) and (r2,c2). If they are diagonal to each other, then
;    r1-r2 and c1-c2 should match in value (the signs will differ depending
;    on the direction of the diagonal.)

(define (same-diagonal? q1 q2)
  (= (abs (- (car q1) (car q2)))
     (abs (- (cadr q1) (cadr q2)))))

; Finally, we can combine these two procedures into a single predicate
; that we can use to test for the safety of the new queen (eg. by
; using it in conjunction with the existing queen positions...)

(define (queens-in-check? q1 q2)
  (or (same-row? q1 q2)
      (same-diagonal? q1 q2)))


; So for our safe procedure, we're given the column k (which we can
; use to get the newest queen's position -- it'll be the one with
; k as the 'column' part of the (row,column) pair). We'll define this
; as newest-queen in a let structure, and then simply make sure that
; none of the existing queens are in check with this one.
;
; So we'll remove the newest-queen from the existing positions list,
; and then filter it with our 'queens-in-check?' predicate. If the
; new list is empty, then we are safe.

; (As a novel idea, we could also have mapped the complement of the
; 'queens-in-check?' procedure across the list of the other queen
; positions and then accumulated the logical -and- of those results!
; But I think our first way is more readable, given how we've defined
; everything.)

(define (safe? column positions)
  (let ((newest-queen (car (filter (lambda (position)
                                     (= (cadr position) column))
                                   positions))))
    (null? (filter (lambda (queen) 
                     (queens-in-check? newest-queen queen))
                   (remove-position newest-queen positions)))))

; We'll also define a small utility procedure 'remove-position'
; that returns the list of positions with the given position
; removed (as a complement to our 'adjoin-position' procedure.)

(define (remove-position pos-to-remove positions)
  (filter (lambda (other-position)
            (not (and (= (car pos-to-remove)
                         (car other-position))
                      (= (cadr pos-to-remove)
                         (cadr other-position)))))
          positions))


; And with that, our code should be complete! We can test this on the
; 4x4 and 5x5 cases:

(queens 4)
;> (((3 4) (1 3) (4 2) (2 1)) ((2 4) (4 3) (1 2) (3 1)))
; ie.
;            O O X O                   O X O O
;            X O O O                   O O O X
;            O O O X                   X O O O
;            O X O O                   O O X O

(queens 5)
;> (((4 5) (2 4) (5 3) (3 2) (1 1)) ((3 5) (5 4) (2 3) (4 2) (1 1)) 
;>  ((5 5) (3 4) (1 3) (4 2) (2 1)) ((4 5) (1 4) (3 3) (5 2) (2 1)) 
;>  ((5 5) (2 4) (4 3) (1 2) (3 1)) ((1 5) (4 4) (2 3) (5 2) (3 1)) 
;>  ((2 5) (5 4) (3 3) (1 2) (4 1)) ((1 5) (3 4) (5 3) (2 2) (4 1)) 
;>  ((3 5) (1 4) (4 3) (2 2) (5 1)) ((2 5) (4 4) (1 3) (3 2) (5 1)))
; ie.
;      X O O O O   X O O O O   O O X O O   O O O X O   O X O O O
;      O O O X O   O O X O O   X O O O O   X O O O O   O O O X O
;      O X O O O   O O O O X   O O O X O   O O X O O   X O O O O
;      O O O O X   O X O O O   O X O O O   O O O O X   O O X O O
;      O O X O O   O O O X O   O O O O X   O X O O O   O O O O X
;
;      O O O O X   O X O O O   O O O O X   O O O X O   O O X O O
;      O O X O O   O O O O X   O X O O O   O X O O O   O O O O X
;      X O O O O   O O X O O   O O O X O   O O O O X   O X O O O
;      O O O X O   X O O O O   X O O O O   O O X O O   O O O X O
;      O X O O O   O O O X O   O O X O O   X O O O O   X O O O O
