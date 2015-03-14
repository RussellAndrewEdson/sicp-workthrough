;;; SICP Exercise 2.69
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the definition of the 
;;; 'generate-huffman-tree' procedure that takes a list of
;;; symbol-frequency pairs and generates the Huffman encoding tree
;;; according to the Huffman algorithm.

; The procedures that we've seen/defined for the Huffman encoding trees
; so far are included below.

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (symbol-in-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (symbol-in-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (if (symbol-in-set? symbol (symbols tree))
      (if (leaf? tree)
          '()
          (let ((left-symbols (symbols (left-branch tree)))
                (right-symbols (symbols (right-branch tree))))
            (cond ((symbol-in-set? symbol left-symbols)
                   (cons 0 (encode-symbol symbol (left-branch tree))))
                  ((symbol-in-set? symbol right-symbols)
                   (cons 1 (encode-symbol symbol (right-branch tree)))))))
      (error "symbol not in tree -- ENCODE-SYMBOL" symbol)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


; Now we're given the following definition for the generate-huffman-tree
; procedure:

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


; We can see how the make-leaf-set procedure works with the pairs list
; by running it with the example given in the book:

(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))
;> ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))

; So what we essentially want to do is merge the two leaves with the smallest
; weight each time (using the make-code-tree constructor), stopping once we get
; to a single tree at the end.

; ie. on the A,B,C,D example above, we want to:
;   1. Make a code tree with (leaf C 1) on the left and (leaf D 1) on the right
;
;   2. With that code tree made, we want to then take the next smallest (ie.
;      the (leaf B 2), and make a code tree with (leaf B 2) on the left and
;      that previous code tree on the right branch.
;
;   3. With -that- code tree made, we then take the next smallest, which is the
;      (leaf A 4), and make a code tree with (leaf A 4) as the left branch
;      and the previous code tree on the right branch.
;
;   4. At this point there's only one leaf/tree left, so we're done.


; So our algorithm will look like this:
;   1. We check for an empty leaves list first. If so, we just return the 
;      empty list (Note that we should only ever get this case if we were
;      actually passed the empty list to begin with, so this is just making
;      sure that we've covered all our bases here.)
;
;   2. We can then grab the car and cdr of the list, and check whether the
;      cdr is empty. If so, then we've reached our termination condition 
;      (where we have the complete tree), and we return the car.
;
;   3. Otherwise, we can grab the cadr of the list. Now given that we have
;      an ordered list representation, these ought to be the two 
;      smallest-weight elements, so we 'merge' them with the make-code-tree
;      procedure.
;
;   4. Finally, we make a recursive call, with a new list that contains the
;      tree we just made placed in its proper position in the order. We can
;      use the 'adjoin-set' procedure to do this easily.


; And that's the entire algorithm. We can code it as a procedure as follows:

(define (successive-merge leaves)
  (cond ((null? leaves) '())
        ((null? (cdr leaves)) (car leaves))
        (else 
         (let ((merged-tree (make-code-tree (car leaves)
                                            (cadr leaves))))
           (successive-merge (adjoin-set merged-tree
                                         (cddr leaves)))))))


; We can test this by comparing it to the sample-tree given in the book.
; The Huffman encoding tree should match up exactly:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

sample-tree
;> ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
;> ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
