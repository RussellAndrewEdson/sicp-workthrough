;;; SICP Exercise 2.89
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise, we defined the procedures required for the
;;; term-list representation of dense polynomials.


; We eventually want to restructure the polynomial system so that
; we have two different representations for polynomials: dense and
; sparse. We've already got the sparse representation done, so we'll
; define the procedures for the dense representation on their own here.

; For reference, the procedures for the term-list for the sparse
; polynomials are included here (note that we still represent
; a polynomial in terms of a cons cell containing the variable symbol
; and the term-list, so none of those other procedures need to change.)

;;  (define (adjoin-term term term-list)
;;    (if (=zero? (coeff term))
;;        term-list
;;        (cons term term-list)))
;;  (define (the-empty-termlist) '())
;;  (define (first-term term-list) (car term-list))
;;  (define (rest-terms term-list) (cdr term-list))
;;  (define (empty-termlist? term-list) (null? term-list))
;;  (define (make-term order coeff) (list order coeff))
;;  (define (order term) (car term))
;;  (define (coeff term) (cadr term))

; So we want to make sure we've got analogous implementations of
; all of these procedures for the dense polynomial representation.


; First, we should note that even though the term-list won't
; explicitly keep track of the order of the terms, the concept of
; representing a term's order together with its coefficient
; in a list was still a nice idea. So what we can do is
; modify the first-term procedure so that it adds the order
; of the term back on after it takes it from the list.
;
; As such, the term constructors and selectors don't even need
; to change here:

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (first-term term-list)
  (let ((order-term (- (length term-list) 1)))
    (make-term order-term (car term-list))))

; Then the procedures for the rest of the terms and the
; empty termlist don't need to change either:

(define (rest-terms term-list) (cdr term-list))
(define (the-empty-termlist) '())
(define (empty-termlist? term-list) (null? term-list))


; So the main procedure we need to modify is the adjoin-term
; procedure. 
;
; Now for adjoining a term to our dense polynomial representaton,
; notice that we might need to have lots of zero terms, depending
; on the order of the given term. So it'd be convenient to define
; a procedure that generates a list of n zeros, like this:

(define (zeros n)
  (define (iter count result)
    (if (= count 0)
        result
        (iter (- count 1) (cons 0 result))))
  (iter n '()))

; So we can use this procedure as follows:

(zeros 5)
;> (0 0 0 0 0)

; Then we can write our adjoin-term procedure like this. Suppose our
; current term-list has N terms in it, and we want to add on an 
; nth-order term:
;   - if the term-list is empty (ie. N=0), then the new term-list is
;     the order of our current term, n, and then a list of n zeros.
;     (This nicely accounts for the 0th-order term, too.)
;
;   - if the term-list is nonempty with N elements, then we look at
;     N-n:
;       - if N-n < 0, then our term has a higher-order than the total
;         number of terms in the list (which is +1 of the highest-order
;         term). So our new term list is the term, followed by |N-n| zeros,
;         followed by the rest of the elements.
;
;       - if N-n = 0, then we don't need any zeros, and simply cons our
;         term onto the front of the existing term-list.
;
;       - if N-n > 0, then we need to insert the coefficient of the term 
;         somewhere in the existing list. But since we're guaranteed that 
;         our list is padded with the necessary zeros at this point, all 
;         we need to do is cdr down the list (N-n)-1 times and replace the 
;         coefficient at that point. (This also handles duplicate terms in 
;         a neat way, particularly when compared with our existing
;         sparse representation.)
;
; And we'll still check for a zero coefficient at the start, so we can
; simply return the term-list in that case.
;
; We can write the procedure like this:

(define (adjoin-term term term-list)
  (define (replace-term-at n)
    (define (iter count first-half second-half)
      (if (= count 0)
          (append first-half 
                  (cons (coeff term) (cdr second-half)))
          (iter (- count 1) 
                (append first-half (list (car second-half)))
                (cdr second-half))))
    (iter n '() term-list))
  (if (=zero? (coeff term))
      term-list
      (let ((order-difference (- (length term-list) (order term))))
        (if (<= order-difference 0)
            (cons (coeff term) 
                  (append (zeros (abs order-difference)) term-list))
            (replace-term-at (- order-difference 1))))))


; And we can test our procedures (here we'll define a dummy '=zero?'
; function that only works on scheme numbers).

(define (=zero? num) (= num 0))

; Define the polynomial p1(x) = 5x^4 + 2x^3 - 4:
(define p1 '(5 2 0 0 -4))

; Adjoining the term x^11:
(adjoin-term '(11 1) p1)
;> (1 0 0 0 0 0 0 5 2 0 0 -4)

; Adjoining the term 7x^5:
(adjoin-term '(5 7) p1)
;> (7 5 2 0 0 -4)

; Adjoining (ie. 'replacing with') the term 6x^2:
(adjoin-term '(2 6) p1)
;> (5 2 6 0 -4)

; Adjoining (ie. 'replacing with') the term 5x^0:
(adjoin-term '(0 5) p1)
;> (5 2 0 0 5)