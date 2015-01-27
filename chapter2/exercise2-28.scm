;;; SICP Exercise 2.28
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us constructing a procedure 'fringe' that takes
;;; in a list and returns a list of all of the leaves of the tree as we
;;; go from left to right.


; Basically, what we want to do here is cdr down the tree, but make sure
; that we "car down" to the lowest branch possible at each point. So we
; want to do the following:
;  - given a list, we'll check out the car of the list (after we make sure
;    the list isn't empty, of course!)
;  - if the car is a pair, then we've got a sublist -- we'll recursively
;    get the fringe of the sublist first and then append it to our final
;    result.
;  - if the car is not a pair, it's a leaf, so we add it to the fringe list.
;    We do need to enclose this in a list though (so that our append operation
;    works the way we want it to.)

; This procedure, when applied to the tree, should generate the entire
; fringe list for us.

; So our procedure is as follows (implemented as an iterative process):

(define (fringe tree)
  (define (fringe-iter subtree leaves)
    (if (null? subtree)
        leaves
        (fringe-iter (cdr subtree)
                     (append leaves (if (pair? (car subtree))
                                        (fringe-iter (car subtree) nil)
                                        (list (car subtree)))))))
  (fringe-iter tree nil))


; We can test it on the given example from the book:

(define x (list (list 1 2) (list 3 4)))

(fringe x)
;> {mcons 1 {mcons 2 {mcons 3 {mcons 4 '()}}}}

(fringe (list x x))
;> {mcons 1 {mcons 2 {mcons 3 {mcons 4 {mcons 1 {mcons 2 {mcons 3 {mcons 4 '()}}}}}}}}


; And just for fun, a slightly more interesting example:

(define y (list (list 7 (list 2 2) 4 (list 3)) 2 (list 7 5)))

(fringe y)
;> {mcons 7 {mcons 2 {mcons 2 {mcons 4 {mcons 3 {mcons 2 {mcons 7 {mcons 5 '()}}}}}}}}
