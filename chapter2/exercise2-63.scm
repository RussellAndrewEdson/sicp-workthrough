;;; SICP Exercise 2.63
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we examined some procedures that transform binary
;;; trees into lists and observed their behaviour and efficiency.


; We have the binary tree data abstraction with its constructors and
; selectors as follows:

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


; And the two procedures for converting binary trees to lists are:

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


; Now for starters, we can notice that our tree->list-1 procedure
; appears to work out the entire left-branch of the tree first, then
; appends that to the results for the right-side of the tree. So we
; seem to work from left-to-right across the binary tree.
;
; On the other hand, with our tree->list-2 procedure, we end up
; evaluating the right-branch of the tree first (recall our
; applicative-order evaluation: we need to fully evaluate that nested
; call to copy-to-list before we start putting the list together!)


; a)
; Let's investigate this by testing the procedures with the trees from
; Figure 2.16 in the book:

(define tree-1 (make-tree 7
                          (make-tree 3
                                     (make-tree 1 '() '())
                                     (make-tree 5 '() '()))
                          (make-tree 9
                                     '()
                                     (make-tree 11 '() '()))))

(define tree-2 (make-tree 3
                          (make-tree 1 '() '())
                          (make-tree 7
                                     (make-tree 5 '() '())
                                     (make-tree 9
                                                '()
                                                (make-tree 11 '() '())))))

(define tree-3 (make-tree 5
                          (make-tree 3
                                     (make-tree 1 '() '())
                                     '())
                          (make-tree 9
                                     (make-tree 7 '() '())
                                     (make-tree 11 '() '()))))


; For tree-1, our procedures return the following lists:

(tree->list-1 tree-1)
;> (1 3 5 7 9 11)

(tree->list-2 tree-1)
;> (1 3 5 7 9 11)

; So we do seem to get the same resulting lists here.

; How about tree-2?

(tree->list-1 tree-2)
;> (1 3 5 7 9 11)

(tree->list-2 tree-2)
;> (1 3 5 7 9 11)

; Again, we have the same list returned.

; Finally, let's look at tree-3:

(tree->list-1 tree-3)
;> (1 3 5 7 9 11)

(tree->list-2 tree-3)
;> (1 3 5 7 9 11)

; So for all of these trees, we do actually get the same result.

; So far, so good. But do the procedures produce the same list for
; every tree?

; Now even though the procedures look different, they should actually
; -always- return the same tree. The overall idea between the two 
; procedures is the same: to construct the list, we separate the tree
; into its left-branch and the stuff on the right (ie. the entry, and
; the right-branch) and work on each part individually, concatenating 
; the sub-lists together at the end. Given this, the way that the list
; actually gets pieced together at the end of each of these steps 
; doesn't make a difference to the result.


; b)
; However, the way the list gets pieced together may indeed make a
; difference to the order of growth in the number of steps for
; each procedure!

; Let's look at how the tree->list-1 procedure works. Suppose we
; have a balanced tree with n elements. Then our algorithm behaves
; as follows:
;   - We apply the 'append' procedure to the result of converting
;     the left-branch (approx n/2 elements) to the result of consing
;     the top element to the conversion of the right-branch (n/2 elements).
; 
;   - Because of the way our applicative order evaluation works, 
;     we've already completely converted the left-branch before we 
;     start work on the right-branch.
; 
;   - So we have order of growth Theta(log n) for the number of steps
;     required to work through each of the two branches.
;
;   - But wait! Notice that every time we're calling that append procedure,
;     which has a Theta(m) order of growth in the number of steps, where
;     m is the number of elements in the first list. At each point in the
;     recursion, that first list will be the elements of the left-branch for
;     that level, which is approximately (n/2) if we had n elements in the
;     sub-tree. So we have an order of growth of Theta(n) for the append
;     operation.
;     
; So we have an order of growth of Theta(log n) for the number of steps
; in the procedure, but at each step we fire off a call to the append
; procedure which has an order of growth Theta(n). So together, we have
; an order of growth Theta(n log n) for the tree->list-1 procedure.


; For tree->list-2 with a given balanced tree of n elements, we have
; the following behaviour:
;   - We copy-to-list the entire left-branch of the tree (n/2 elements)
;     to the result of consing the top element to the copy-to-list conversion
;     of the entire right-branch (n/2 elements).
;
;   - At each point in those recursive calls though, we're basically just
;     consing on the elements one by one. So for the left-branch, we expect
;     n/2 calls to cons (a primitive procedure, so we'll assume an order of
;     growth Theta(1)), and we expect n/2 calls to cons for the right-branch
;     too.
;
; So all up, we have an order of growth Theta(n) in the number of steps
; for a tree with n elements (that is, if we add 100 elements to the
; tree [and it's still balanced], we expect to make 100 more calls to
; cons.)

; Then we can see that the tree->list-2 procedure grows more slowly
; (ie. is more efficient in terms of number of steps required) than the
; tree->list-1 procedure, since n*log2(n) > n for any n > 2. 
