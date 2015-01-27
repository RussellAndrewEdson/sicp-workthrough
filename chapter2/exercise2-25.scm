;;; SICP Exercise 2.25
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us giving combinations of cars and cdrs to 
;;; retrieve elements from some nested list structures.

; We want to retrieve 7 from each of the following lists.
;
; Note: For each list, I'm assuming that all 'sub-lists' are
; actually lists themselves; that is, the expression (1 (2 3))
; for instance is constructed as (list 1 (list 2 3)), and -not-
; as (cons 1 (cons 2 3)). 


; For the list (1 3 (5 7) 9), we want the:
;  - cdr: (3 (5 7) 9)
;  - cdr: ((5 7) 9)
;  - car: (5 7)
;  - cdr: (7)
;  - car: 7.

; So we can write an expression to do exactly that:

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
;> 7

; We also could have used the cdaddr shorthand here:

(car (cdaddr (list 1 3 (list 5 7) 9)))
;> 7


; For the list ((7)), we want the:
;  - car: (7)
;  - car: 7.

(car (car (list (list 7))))
;> 7

; Or alternatively:

(caar (list (list 7)))
;> 7


; This last example is tricky: we need to keep in mind that upon each
; cdr, we actually get a pair -- the nested list in the car, and the
; empty list as the cdr. 
;
; So to "cdr down the list", we'll actually need to cdr and then car to get
; to the next level of the nested list.
;
; That is, for (1 (2 (3 (4 (5 (6 7)))))), we want the:
;  - cdr: ((2 (3 (4 (5 (6 7))))))
;  - car: (2 (3 (4 (5 (6 7)))))
;  - cdr: ((3 (4 (5 (6 7)))))
;  - car: (3 (4 (5 (6 7))))
;  - cdr: ((4 (5 (6 7))))
;  - car: (4 (5 (6 7)))
;  - cdr: ((5 (6 7)))
;  - car: (5 (6 7))
;  - cdr: ((6 7))
;  - car: (6 7)
;  - cdr: (7)
;  - car: 7.

(define lst (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr lst))))))))))))
;> 7

; Alternatively, using the shorthand:

(cadadr (cadadr (cadadr lst)))
;> 7