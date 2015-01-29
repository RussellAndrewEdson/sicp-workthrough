;;; SICP Exercise 2.31
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we abstracted our square-tree procedure from
;;; Exercise 2.30 to define the tree-map procedure that takes in a
;;; procedure and a tree and applies that procedure to each leaf.


; Our code from Exercise 2.30 was as follows:

;;  (define (square-tree tree)
;;    (map (lambda (sub-tree)
;;           (if (pair? sub-tree)
;;               (square-tree sub-tree)
;;               (square sub-tree)))
;;         tree))


; We can simply take out the reference to the 'square' procedure and
; define our 'tree-map' procedure in terms of an input function f
; instead:

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))


; Then we can define our old square-tree procedure in terms of this:

(define (square x)
  (* x x))

(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;> (1 (4 (9 16) 25) (36 49))
