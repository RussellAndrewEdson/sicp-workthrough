;;; SICP Exercise 2.43
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we investigated the effects of interchanging
;;; the order of the nested mappings in the code for the n-queens puzzle
;;; from Exercise 2.42.

; As given, we have two different orders for the nested mappings, where 
; one apparently runs much slower than the other. So we'll bring in all
; of the ancillary procedure definitions for the n-queens puzzle from 
; the last exercise (note that we're signaling for a call to flatmap
; in the output -- this will be useful for testing purposes.)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (display "flatmap called")
  (newline)
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

(define (adjoin-position row column positions)
  (cons (list row column) positions))

(define empty-board nil)

(define (same-row? q1 q2)
  (= (car q1) (car q2)))

(define (same-diagonal? q1 q2)
  (= (abs (- (car q1) (car q2)))
     (abs (- (cadr q1) (cadr q2)))))

(define (queens-in-check? q1 q2)
  (or (same-row? q1 q2)
      (same-diagonal? q1 q2)))

(define (safe? column positions)
  (let ((newest-queen (car (filter (lambda (position)
                                     (= (cadr position) column))
                                   positions))))
    (null? (filter (lambda (queen) 
                     (queens-in-check? newest-queen queen))
                   (remove-position newest-queen positions)))))

(define (remove-position pos-to-remove positions)
  (filter (lambda (other-position)
            (not (and (= (car pos-to-remove)
                         (car other-position))
                      (= (cadr pos-to-remove)
                         (cadr other-position)))))
          positions))


; So we'll go ahead and define the main 'queens' procedure, 
; but this time we'll define it twice: one that uses the original
; nesting order, and the other that uses the new (slower?) order.
; 
; Now if swapping the order is going to have any effect on the
; speed, our first suspect should be that recusrive call to the
; queen-cols procedure, and when exactly it gets called with respect
; to the flatmap procedure. So we'll add some display statements in there
; so we can see what's happening, too. 

(define (queens-fast board-size)
  (define (queen-cols k)
    (display "queen-cols called with k=")
    (display k)
    (newline)
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

(define (queens-slow board-size)
  (define (queen-cols k)
    (display "queen-cols called with k=")
    (display k)
    (newline)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


; Let's explore this, shall we? Since we're not worried about the
; output and we want some easy cases, we'll look at the cases for
; board-sizes of 2 and 3 (which have no actual solutions, so our
; output won't get cluttered.)

; Calling queens-fast with a board-size of 2 gives us the following
; call stack:

(queens-fast 2)
;> queen-cols called with k=2
;> queen-cols called with k=1
;> queen-cols called with k=0
;> flatmap called
;> flatmap called
;> '()

; And for the board-size of 3, we have the following:

(queens-fast 3)
;> queen-cols called with k=3
;> queen-cols called with k=2
;> queen-cols called with k=1
;> queen-cols called with k=0
;> flatmap called
;> flatmap called
;> flatmap called
;> '()

; So far, so good -- we have a nice direct proportionality between
; the board-size and the number of calls that are made to the queen-cols
; procedure here. Also notice that recursion has completed fully before
; we actually make any calls to the flatmap procedure.


; But let's check the slower procedure now:

(queens-slow 2)
;> queen-cols called with k=2
;> flatmap called
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> '()


; Uh-oh! What about for a board-size of 3?
; (You'll want to be scrolling down quite a bit for this one.)

(queens-slow 3)
;> queen-cols called with k=3
;> flatmap called
;> queen-cols called with k=2
;> flatmap called
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=2
;> flatmap called
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=2
;> flatmap called
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=1
;> flatmap called
;> queen-cols called with k=0
;> queen-cols called with k=0
;> queen-cols called with k=0
;> '()

; Wow. No wonder this second nesting makes the program run slower!

; The problem, as you can probably see by now, is that we appear to
; have a pattern of tree-recursion happening with this slower version.
; If we sum up the calls, calling (queens-slow 3) fires off:
;   - 1  call  to (queen-cols 3)
;   - 3  calls to (queen-cols 2)
;   - 9  calls to (queen-cols 1)
;   - 27 calls to (queen-cols 0).

; By including the calls to flatmap in the display output, we can get a
; feel for why this is happening. Recall our substitution model -- we only 
; determine the value of that outer lambda expression when its actually applied.
; In our case here, we're not actually determining the values for 
; (queen-cols (- k 1)) until we've stepped into a flatmap call and have applied
; the procedure to a single element of the interval enumeration. And this is a bit 
; of a problem, since we end up re-evaluating the lambda for -every- element
; in the interval enumeration, for -every- value of k we ever see!

; As an illustration, consider the smaller (queens 2) case. The following
; sequence of procedure calls happens:
;
;   -  We call queen-cols with k=2. In that procedure, we call flatmap with
;      the outer lambda expression and the enumeration interval (1 2):
;
;        1. We apply the lambda to the first element, 1. This requires an
;           evaluation of the lambda, so we need to fire off a call to 
;           queen-cols with k=1. In that procedure, we call flatmap with
;           the outer lambda expression and the sequence (1 2):
;
;             1a. We apply the lambda to the first element 1, which requires
;                 an evaluation of the lambda. So we fire off a call to
;                 queen-cols with k=0, which returns the empty-board.
;
;             1b. We apply the lambda to the second element 2, which requires
;                 an evaluation of the lambda. So we fire off a call to
;                 queen-cols with k=0, which returns the empty-board.
;
;        2. We then apply the lambda to the second element, 2. This requires
;           an evaluation of the lamda, so we need to fire off a call to
;           queen-cols with k=1. In that procedure, we again call flatmap
;           with the outer lambda expression and the sequence (1 2):
;
;             2a. We apply the lambda to the first element 1, which requires
;                 an evaluation of the lambda. So we fire off a call to
;                 queen-cols with k=0, which returns the empty-board.
;
;             2b. We apply the lambda to the second element 2, which requires
;                 an evaluation of the lambda. So we fire off a call to
;                 queen-cols with k=0, which returns the empty-board.

; So you can see how this results in the sequence of calls we see in the
; output above.


; Then based on that pattern that we saw earlier, we can make an estimate
; for how long it would take the slower program to solve the 8-queens 
; puzzle. Suppose we assume that the faster version solves the problem in
; time T. We'll make the extra assumption that the calls to (queen-cols 0)
; are negligible in either case (since all that does is return the empty
; list.)

; So our faster version takes time T, and has 8 calls to queen-cols.
; (eg. (queen-cols 8) -> (queen-cols 7) -> ... -> (queen-cols 1). That is,
; one call for each row.

; Then our slow version has:
;   - 1 call to (queen-cols 8)
;   - 8 calls to (queen-cols 7)
;   - 8^2 = 64 calls to (queen-cols 6)
;   - 8^3 = 512 calls to (queen-cols 5)
;   ...
;   - 8^7 = 2,097,152 calls to (queen-cols 1).

; All up, thats 1 + 8^1 + 8^2 + 8^3 + ... + 8^7 = 2,396,745 calls!
; (versus 8 calls for the fast method.) We can in fact write this
; summation in an algebraically equivalent form: (1/7)*(8^8 - 1).

; So if we make the assumption that all of the calls to (queen-cols k)
; for k=1 to 8 basically take the same amount of time (recall that the
; queen-cols procedure also filters the list for the "safe" solutions,
; so these lists don't actually grow too ridiculous too fast..), then if 
; the first method takes time T with 8 calls, our slower method runs on 
; the order of (1/7)*(8^7)*T ! 
