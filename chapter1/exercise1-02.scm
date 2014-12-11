;;; SICP Exercise 1.2
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us translating the expression:
;;;    (5 + 4 + (2 - (3 - (6 + (4/5) )))) / (3 * (6 - 2) * (2 - 7))
;;;
;;;  into prefix form. (It's worth noting that the actual expression
;;;  from the online text is kind of hard to read because it uses a
;;;  low-resolution image. I'm pretty sure those are '4's, anyway.)
;;;  EDIT: My hard-copy of the book arrived today, so can confirm that
;;;  those are indeed '4's.

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;> -37/150
