;;; SICP Exercise 2.26
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we looked at two lists (1 2 3) (4 5 6), and
;;; examined the interpreter response to evaluating a combination
;;; of the two lists.

; So we have the lists:

(define x (list 1 2 3))
(define y (list 4 5 6))


; When we append the lists x and y, we get one single list that 
; contains the elements of x followed by the elements of y, or
; the list (1 2 3 4 5 6).

; Now keeping in mind that lists are represented as linked cons-cells,
; the interpreter will return us a structure like the following:
;   (1 (2 (3 (4 (5 (6 ()))))))

(append x y)
;> {mcons 1 {mcons 2 {mcons 3 {mcons 4 {mcons 5 {mcons 6 '()}}}}}}


; When we cons the lists x and y, we end up with a cons cell with
; its car as the list x, and its cdr as the list y, ie. something
; like ((1 2 3) (4 5 6)).
;
; As the interpreter will read the lists x and y in linked cons-cell
; form, then we'll get the following structure:
;   ((1 (2 (3 ()))) (4 (5 (6 ()))))

(cons x y)
;> {mcons {mcons 1 {mcons 2 {mcons 3 '()}}} {mcons 4 {mcons 5 {mcons 6 '()}}}}


; Finally, when we call list with the arguments x and y, we'll get
; a list with the first element as x, and the second element as y 
; -- which when represented in terms of pairs, means that the car 
; of the list will be x, and the cdr of the list will be the pair 
; (y, ()), so we'll have something like ((1 2 3) ((4 5 6) ())).

; That is, the interpreter will return us the structure:
;   ((1 (2 (3 ()))) ((4 (5 (6 ()))) ()))

(list x y)
;> {mcons {mcons 1 {mcons 2 {mcons 3 '()}}} {mcons {mcons 4 {mcons 5 {mcons 6 '()}}} '()}}

