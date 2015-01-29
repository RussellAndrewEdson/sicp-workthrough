;;; SICP Exercise 2.30
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we devised the procedure square-tree to square
;;; the numbers of a given tree structure similar to what we did
;;; in Exercise 2.21. We defined the procedure both directly and in
;;; terms of a recursive map operation.

; Our old code from Exercise 2.21 is included here for reference:

;;  (define (square-list-recursive items)
;;    (if (null? items)
;;        nil
;;        (cons (square (car items)) 
;;              (square-list-recursive (cdr items)))))

;;  (define (square-list items)
;;    (map square items))

; Our code in this exercise will pretty much be a cross between this
; old code and the scale-tree example from the book.


; First, we'll construct the direct procedure. We'll first check that
; we don't have the empty list, as always. Then we want to check
; for whether we have a tree (ie. pair? returns true) or not. If we
; don't have a tree, we must have a leaf (ie. a number), so we square it.
; If we do have a tree, we return a new tree constructed by applying the
; square-tree procedure to both the car and the cdr.

(define (square x)
  (* x x))

(define (square-tree-direct tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-direct (car tree))
                    (square-tree-direct (cdr tree))))))

; We can test the procedure with the given example:

(square-tree-direct
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;> (1 (4 (9 16) 25) (36 49))


; Next, we simply use the higher-order map procedure recursively:

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

; We should also test this procedure:

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;> (1 (4 (9 16) 25) (36 49))
