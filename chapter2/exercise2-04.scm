;;; SICP Exercise 2.4
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us considering a procedural representation of pairs,
;;; where we used our substitution model to verify that it satisfies the
;;; required conditions.

; So our cons and car procedures are defined as follows:

;;  (define (cons x y)
;;    (lambda (m) (m x y)))

;;  (define (car z)
;;    (z (lambda (p q) p)))


; So let's see what happens when we evaluate (car (cons x y)).
; For this to be a valid pair implementation, we ought to get back x:

;;  (car (cons x y))
;;  (car (lambda (m) (m x y)))
;;  ((lambda (m) (m x y)) (lambda (p q) p))
;;  ((lambda (p q) p) x y)
;;  x

; Sure enough, we get x back from the 'pair', as required.


; Then we can see that the corresponding definition for cdr is:

;;  (define (cdr z)
;;    (z (lambda (p q) q)))

; Applying the substitution model to (cdr (cons x y)) gives us y:

;;  (cdr (cons x y))
;;  (cdr (lambda (m) (m x y)))
;;  ((lambda (m) (m x y)) (lambda (p q) q))
;;  ((lambda (p q) q) x y)
;;  y
