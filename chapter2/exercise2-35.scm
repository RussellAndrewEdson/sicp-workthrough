;;; SICP Exercise 2.35
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us redefining the count-leaves procedure from
;;; the book as an accumulation.

; We have the previous definition for the count-leaves procedure:

;;  (define (count-leaves x)
;;    (cond ((null? x) 0)
;;          ((not (pair? x)) 1)
;;          (else (+ (count-leaves (car x))
;;                   (count-leaves (cdr x))))))


; Now for our new accumulation definition, we'll use our accumulate
; procedure:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; Now we are given the hint that for the sequence argument of the 
; accumulate procedure call, we want to map something. So given that
; we can look at a tree as a list of elements that may or may not be
; trees themselves, we can define a procedure that will return the
; number of leaves of each of these subtrees by recursively calling the
; count-leaves procedure itself. Then adding these up (using an initial 
; value of 0) should give us what we need.
;
; This is, in fact, pretty much what we did in the previous procedure
; anyway:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (subtree)
                         (if (not (pair? subtree))
                             1
                             (count-leaves subtree)))
                       t)))


; We can test our new count-leaves procedure with the examples
; from the book:

(define x (cons (list 1 2) (list 3 4)))
(count-leaves x)
;> 4

(count-leaves (list x x))
;> 8