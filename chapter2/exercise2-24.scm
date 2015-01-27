;;; SICP Exercise 2.24
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we evaluated a nested list expression, and also
;;; drew up the box-and-pointer diagram and the tree structure
;;; (see exercise2-24_diagrams.png)


; So we want to evaluate the nested list expression 
;   (list 1 (list 2 (list 3 4))) .

; We can easily get a grasp of what's going on by informally running
; it through with our substitution model in mind (ie. we evaluate the
; arguments first, etc.)

; Here I've represented the result lists in a readable (1 2) form.
; Keep in mind though that this isn't how they look in code!
; (Otherwise the interpreter reads (1 2) and tries to apply the procedure
; '1' to the single argument '2'...)

;;  (list 1 (list 2 (list 3 4)))
;;  (list 1 (list 2 (3 4)))
;;  (list 1 (2 (3 4)))
;;  (1 (2 (3 4)))

; So we end up with the list that looks like (1 (2 (3 4))).

; Of course, the interpreter actually represents this list structure in
; terms of linked pairs, where the last pair in a list has nil as
; its cdr. So it'll look something more like this:

;;  (1 ((2 ((3 (4 nil)) nil)) nil))

; We can verify this by actually running the code:

(list 1 (list 2 (list 3 4)))
;> {mcons 1 {mcons {mcons 2 {mcons {mcons 3 {mcons 4 '()}} '()}} '()}}


; (See the other file, exercise2-24_diagrams.png for the box-and-pointer
; structure and the tree representation.)