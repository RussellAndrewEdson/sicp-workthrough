;;; SICP Exercise 2.41
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wrote a procedure to generate all of
;;; the ordered triples 1 <= i,j,k <= n that sum to a given 
;;; integer s.

; We'll bring in those useful enumeration and map/filter/accumulate
; procedures:

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; Now assuming we have a procedure 'ordered-triples' that
; returns us a list of all the ordered triples (i,j,k) with
; 1 <= i,j,k <= n, then all we need to do is filter the ones
; that sum to a given integer s.

(define (sum-triples n s)
  (define (sum-to-s? triple)
    (let ((i (car triple))
          (j (cadr triple))
          (k (caddr triple)))
      (= (+ i j k) s)))
  (filter sum-to-s? (ordered-triples n)))


; Now for our ordered-triples procedure, we want a nested mapping
; where we enumerate the numbers from 1 to n for i,j,k, and accumulate
; the ordered triples with our flatmap procedure similar to how it's
; done in the book:

(define (ordered-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map (lambda (k) (list i j k))
             (enumerate-interval 1 n)))
      (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))

; And this procedure works as follows:

(ordered-triples 3)
;> ((1 1 1) (1 1 2) (1 1 3) (1 2 1) (1 2 2) (1 2 3) (1 3 1) (1 3 2) (1 3 3) 
;>  (2 1 1) (2 1 2) (2 1 3) (2 2 1) (2 2 2) (2 2 3) (2 3 1) (2 3 2) (2 3 3) 
;>  (3 1 1) (3 1 2) (3 1 3) (3 2 1) (3 2 2) (3 2 3) (3 3 1) (3 3 2) (3 3 3))


; So our sum-triples procedure should work too.

(sum-triples 5 9)
;> ((1 3 5) (1 4 4) (1 5 3) (2 2 5) (2 3 4) (2 4 3) (2 5 2) (3 1 5) (3 2 4) 
;>  (3 3 3) (3 4 2) (3 5 1) (4 1 4) (4 2 3) (4 3 2) (4 4 1) (5 1 3) (5 2 2) 
;>  (5 3 1))