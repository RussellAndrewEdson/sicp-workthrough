;;; SICP Exercise 2.21
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the two different definitions
;;; of the square-list procedure that were given in the book.


; We'll use the usual square procedure that we always have:

(define (square x)
  (* x x))


; The first definition is in terms of a normal recursive process:

(define (square-list-recursive items)
  (if (null? items)
      nil
      (cons (square (car items)) 
            (square-list-recursive (cdr items)))))

; This definition works as expected:

(square-list-recursive (list 1 2 3 4 5 6 7))
;> {mcons 1 {mcons 4 {mcons 9 {mcons 16 {mcons 25 {mcons 36 {mcons 49 '()}}}}}}}


; For the second definition, we'll write it in terms of the 
; higher-order map function instead:

(define (square-list items)
  (map square items))

; We get the exact same result:

(square-list (list 1 2 3 4 5 6 7))
;> {mcons 1 {mcons 4 {mcons 9 {mcons 16 {mcons 25 {mcons 36 {mcons 49 '()}}}}}}}