;;; SICP Exercise 2.66
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the 'lookup' procedure for
;;; the binary-tree representation of the set of records ordered
;;; by the numerical values of the keys.

; We'll bring in our binary tree abstraction code again:

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

; And just so that we can get the code to work and test it, we'll define
; a trivial data abstraction for a "record", which will just consist of a 
; cons cell with a 'key' and a 'value'. The constructor and selectors are 
; as follows:

(define (make-record key value) (cons key value))

(define (key record) (car record))

(define (value record) (cdr record))


; Now suppose we organise the records as a binary tree ordered by
; the numerical values of the keys. What we want to have happen with
; our lookup procedure is the following:
;   - if the given tree is empty, we'll return false.
;   - otherwise, we'll check the key for the current entry of the tree.
;     If it is equal to the given key, we'll return that entry.
;   - if not, then we'll compare the value of the keys. If the key we
;     want is -smaller- than the entry's key, we'll search in the 
;     left-branch. If it is -larger-, we'll search in the right-branch.
;
; Notice that this algorithm is very similar to the usual Binary Search 
; algorithm. (In fact, if we are given a -balanced- tree, it's pretty much 
; exactly a binary search.)

(define (lookup given-key tree-of-records)
  (if (null? tree-of-records)
      false
      (let ((entry-key (key (entry tree-of-records))))
        (cond ((equal? given-key entry-key)
               (entry tree-of-records))
              ((< given-key entry-key)
               (lookup given-key (left-branch tree-of-records)))
              ((> given-key entry-key)
               (lookup given-key (right-branch tree-of-records)))))))


; And as always, we can test our procedure with a small example
; (a binary tree with some values that spell out "record", in order.)

(define records (make-tree (make-record 5 'o)
                           (make-tree (make-record 3 'e)
                                      (make-tree (make-record 1 'r) '() '())
                                      (make-tree (make-record 4 'c) '() '()))
                           (make-tree (make-record 7 'r)
                                      '()
                                      (make-tree (make-record 8 'd) '() '()))))

(lookup 4 records)
;> (4 . c)

(lookup 8 records)
;> (8 . d)

(lookup 17 records)
;> #f