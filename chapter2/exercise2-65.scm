;;; SICP Exercise 2.65
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used the results from Exercises 2.63 and 
;;; 2.64 to provide Theta(n) implementations of the union-set 
;;; and intersection-set operations for sets implemented as 
;;; balanced binary trees.


; So we have our binary tree selectors and constructors as follows:

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


; Now a straight-forward way to get Theta(n) implementations for the
; union-set and intersection-set operations is to simply use our
; procedures from Exercises 2.63 and 2.64 to first convert the given
; trees into ordered lists (if we choose our tree->list2 procedure, this
; is a Theta(n) operation), then perform the union/intersection (which
; also have order of growth Theta(n)), and finally convert back to a
; balanced binary tree (another Theta(n) operation.)
;
; So all up, we have 3 operations with order of growth Theta(n) in the
; number of steps for a set of n elements. That is, we have an order
; of growth Theta(3*n) for the entire procedure, which becomes Theta(n)
; since we can disregard that constant 3.

; So all of the procedures from previous exercises are shown below
; (including the union-set and intersection-set procedures from 
; Exercise 2.62).
; I've renamed the procedures and bindings somewhat to more accurately 
; reflect their intended purpose here.

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else 
         (let ((x1 (car list1)) (x2 (car list2)))
           (cond ((= x1 x2)
                  (cons x1 (union-list (cdr list1) (cdr list2))))
                 ((< x1 x2)
                  (cons x1 (union-list (cdr list1) list2)))
                 ((> x1 x2)
                  (cons x2 (union-list list1 (cdr list2)))))))))

(define (intersection-list list1 list2)
  (if (or (null? list1) (null? list2))
      '()
      (let ((x1 (car list1)) (x2 (car list2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-list (cdr list1)
                                        (cdr list2))))
              ((< x1 x2)
               (intersection-list (cdr list1) list2))
              ((> x1 x2)
               (intersection-list list1 (cdr list2)))))))


; Then our union-set procedure is written as follows:

(define (union-set set1 set2)
  (list->tree (union-list (tree->list set1)
                          (tree->list set2))))

; Similarly for our intersection-set procedure:

(define (intersection-set set1 set2)
  (list->tree (intersection-list (tree->list set1)
                                 (tree->list set2))))


; We can test our procedures on some simple trees.

(define tree-1 (make-tree 4
                          (make-tree 2 
                                     '()
                                     (make-tree 3 '() '()))
                          (make-tree 6
                                     '()
                                     (make-tree 7 '() '()))))
;> (4 (2 () (3 () ())) (6 () (7 () ())))

(define tree-2 (make-tree 3
                          (make-tree 2
                                     (make-tree 1 '() '())
                                     '())
                          (make-tree 4
                                     '()
                                     (make-tree 5 '() '()))))
;> (3 (2 (1 () ()) ()) (4 () (5 () ())))

(define tree-3 (make-tree 5
                          (make-tree 3
                                     '()
                                     (make-tree 4 '() '()))
                          (make-tree 7
                                     (make-tree 6 '() '())
                                     (make-tree 8 '() '()))))
;> (5 (3 () (4 () ())) (7 (6 () ()) (8 () ())))


(union-set tree-1 tree-2)
;> (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) (7 () ())))

(intersection-set tree-1 tree-2)
;> (3 (2 () ()) (4 () ()))

(display (intersection-set tree-1 tree-3))
;> (4 (3 () ()) (6 () (7 () ())))