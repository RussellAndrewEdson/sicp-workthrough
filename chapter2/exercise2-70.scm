;;; SICP Exercise 2.70
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used our completed Huffman encoding tree
;;; system to create an encoding tree and encode a particular message,
;;; and observed the difference in the number of bits needed to store
;;; the message.

; All of our Huffman encoding tree procedures are included below.

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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (cond ((null? leaves) '())
        ((null? (cdr leaves)) (car leaves))
        (else 
         (let ((merged-tree (make-code-tree (car leaves)
                                            (cadr leaves))))
           (successive-merge (adjoin-set merged-tree
                                         (cddr leaves)))))))


; Now we have the following eight-symbol alphabet (with relative frequencies)
; defined here:

(define alphabet 
  '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

; We can use our genereate-huffman-tree procedure to get the Huffman encoding
; tree for the alphabet.

(define huffman-tree (generate-huffman-tree alphabet))
huffman-tree
;> ((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) 
;>  (WAH BOOM) 2) (A WAH BOOM) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) 
;>  (JOB GET) 4) (SHA JOB GET) 7) (A WAH BOOM SHA JOB GET) 11) 
;>  (YIP A WAH BOOM SHA JOB GET) 20) (NA YIP A WAH BOOM SHA JOB GET) 36)


; The message we want to encode is as follows:
(define message 
  '(GET A JOB
    SHA NA NA NA NA NA NA NA NA
    GET A JOB
    SHA NA NA NA NA NA NA NA NA
    WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
    SHA BOOM))

; So we can encode the message as a string of bits with our 'encode'
; procedure.

(encode message huffman-tree)
;> (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 
;>  1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 
;>  0 1 0 1 1 1 0 1 1 0 1 1)

; Or, in a more "readable" form:
;   11111 1100 11110 
;   1110 0 0 0 0 0 0 0 0 
;   11111 1100 11110 
;   1110 0 0 0 0 0 0 0 0 
;   11010 10 10 10 10 10 10 10 10 10 
;   1110 11011
;
; (See exercise2-70_diagram.png for a drawing of this Huffman tree and
; the encodings for each of the words.)

; Now we have a total of 84 bits here.

; Suppose instead we were to use a fixed-length code instead. We have eight
; different symbols here, so we'd need at least log2(8) = 3 bits for each
; symbol.
;
; So we could have something like this:
;   A = 000,  BOOM = 001,  GET = 010,  JOB = 011,
;   NA = 100, SHA = 101, YIP = 110, WAH = 111.

; Then our message gets encoded to this string of bits instead:
;   010 000 011
;   101 100 100 100 100 100 100 100 100
;   010 000 011
;   101 100 100 100 100 100 100 100 100
;   111 110 110 110 110 110 110 110 110 110
;   101 001

; This encoding of the same message uses 108 bits -- more than the
; Huffman-tree encoding.
