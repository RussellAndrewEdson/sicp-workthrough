;;; SICP Exercise 2.68
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the encoding procedure for the Huffman
;;; encoding tree code.

; We'll bring in all of the other procedures for the Huffman encoding trees:

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


; We're given the encode procedure as follows:

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; We need to implement the 'encode-symbol' procedure. We want it to
; take in the symbol to encode and the Huffman encoding tree to encode
; it by, and return the corresponding list of bits.
;
; Now the encoding procedure works as follows:
;   1. We first check whether the given symbol is among the symbols
;      contained in the tree. If not, we can exit with an error
;      immediately.
;
;   2. Next, we check whether we have a leaf. Note that since we've
;      already determined that the symbol is contained in the symbol
;      set, then if we have a leaf we're done -- we return the code
;      that we've accumulated so far (or, if we want to work this as
;      a recursive process, here's where we'd return '() for the end
;      of the list that we're constructing.)
;
;   3. Otherwise, we must have a tree. So we check the symbols in the
;      left-branch and right-branch to determine which branch to move
;      down next. If we go left, we add a 0 to the code, and we add
;      a 1 to the code if we go right.


; We'll need a way to find a symbol in the symbols set. We can use a similar 
; procedure to any of the "element-of-set?" procedures we saw when dealing with
; sets back in Exercises 2.59, 2.60:

(define (symbol-in-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (symbol-in-set? x (cdr set)))))


; Then we can write our encode-symbol procedure to generate a recursive
; process in the following way:

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


; And we're done. We can test this with the sample-tree and sample-message
; that we were given in Exercise 2.67:

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree 
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; Recall that we decoded this to the message ADABBCA.

(encode '(A D A B B C A) sample-tree)
;> (0 1 1 0 0 1 0 1 0 1 1 1 0)

sample-message
;> (0 1 1 0 0 1 0 1 0 1 1 1 0)

; So we do get back the original bit string, which is a good sign.