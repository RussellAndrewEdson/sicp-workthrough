;;; SICP Exercise 2.39
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the reverse procedure from
;;; Exercise 2.18 in terms of fold-left and fold-right.

; We have the fold-left and fold-right procedures from Exercise 2.38:

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


; Now recall that fold-right combines all of the list elements
; starting from the right-hand side first. So at each point we
; want to append the accumulated list in -front- of the newer element.
; Since we start at the end and work our way back up the list, 
; this will collect everything in the reverse order.

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(reverse-right (list 1 2 3 4 5))
;> (5 4 3 2 1)


; For fold-left on the other hand, we start at the left and accumulate
; elements as we walk down the list from left to right. So at each point
; we want to appen the growing list to the -back- of the newer element,
; which we can actually do with cons:

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(reverse-left (list 1 2 3 4 5))
;> (5 4 3 2 1)