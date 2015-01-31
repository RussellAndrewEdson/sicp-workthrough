;;; SICP Exercise 2.38
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we looked at the subtle difference between
;;; the fold-left and fold-right procedures, and considered the
;;; conditions under which they produce equal results.

; For testing purposes we'll bring in our accumulate procedure
; (renamed to fold-right):

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

; And the fold-left procedure is given as follows (note that as we
; hinted at back in Exercise 2.33, this uses an iterative process and
; as such, we end up "accumulating" from the other end first.)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


; Now consider the expression:
;   (fold-right / 1 (list 1 2 3))
; Since we are combining the elements to the right, we have the sequence of
; operations 1/(2/(3/1)) = 1/(2/3) = 3/2 = 1 1/2. Sure enough:

(fold-right / 1 (list 1 2 3))
;> 1 1/2

; Then the equivalent fold-left expression:
;   (fold-left / 1 (list 1 2 3))
; Does it the other way around: we start with the initial value (1), divide
; it by 1, then the result by 2, then the result by 3, ie.
; ((1/1)/2)/3 = 1/6.

(fold-left / 1 (list 1 2 3))
;> 1/6


; Similarly, the expression:
;   (fold-right list nil (list 1 2 3))
; Will return the result (list 1 (list 2 (list 3 nil))), ie. (1 (2 (3 ()))):

(fold-right list nil (list 1 2 3))
;> (1 (2 (3 ())))

; Then the fold-left expression:
;   (fold-left list nil (list 1 2 3))
; Returns the result of combining from left-to-right, ie.
;   (list (list (list nil 1) 2) 3) = (((() 1) 2) 3):

(fold-left list nil (list 1 2 3))
;> (((() 1) 2) 3)


; So for a given operation op (which we'll denote by * here), working on 
; an example list (1 2 3 4 5) with an initial value 0, we have the following:

; fold-right: (1 * (2 * (3 * (4 * (5 * 0)))))
;  fold-left: (((((0 * 1) * 2) * 3) * 4) * 5)

; So we can see here that we require that the operation op satisfy the 
; associative law.


; * However, we may require more than that too! Depending on what the initial
; value is, we may also require commutativity. Let's bring in the matrix code
; from Exercise 2.37 for an example:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (first-elements seqs)
  (map car seqs))

(define (without-first-elements seqs)
  (map cdr seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (first-elements seqs))
            (accumulate-n op init (without-first-elements seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define mat-a (list (list 1 2) (list 3 4)))
(define mat-b (list (list 1 2) (list -1 3)))
(define mat-c (list (list 1 0) (list 2 -1)))
(define mat-i (list (list 1 0) (list 0 1)))


; Now matrix multiplication is associative, but not commutative.
; If our initial value is the identity matrix we'll be okay
; (since the identity commutes with everything):

(fold-left matrix-*-matrix mat-i (list mat-b mat-c))
;> ((5 -2) (5 -3))

(fold-right matrix-*-matrix mat-i (list mat-b mat-c))
;> ((5 -2) (5 -3))


; But when our initial value is something else, things go a
; little awry:

(fold-left matrix-*-matrix mat-a (list mat-b mat-c))
;> ((15 -8) (35 -18))

(fold-right matrix-*-matrix mat-a (list mat-b mat-c))
;> ((-1 2) (-4 -2))


; This makes sense if we look back up at our algebraic 
; representations for fold-left and fold-right. To transform
; one into the other, we need to move that initial element
; from one side of the expression to the other side:
;   (((((0 * 1) * 2) * 3) * 4) * 5)
;   (0 * 1 * 2 * 3 * 4 * 5)    using associativity
;   (1 * 0 * 2 * 3 * 4 * 5)    0 commutes with 1
;   (1 * 2 * 0 * 3 * 4 * 5)    0 commutes with 2
;    ...                         ...etc
;   (1 * 2 * 3 * 4 * 5 * 0)
;   (1 * (2 * (3 * (4 * (5 * 0)))))

; That is, we need the initial element to commute with every
; element in the sequence. But since we want a condition that
; holds given -any- sequence, then this means that op must be
; commutative (and/or 0 is the identity element for op.)


; So to guarantee that fold-left and fold-right produce the
; same values for -any- given sequence, we require that the operation
; op be associative, and that at least one of the following holds:
;   1. The initial element is the identity with respect to op;
;   2. The operation op is commutative.