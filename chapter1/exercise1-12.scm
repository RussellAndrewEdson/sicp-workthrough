;;; SICP Exercise 1.12
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us writing a procedure to compute the elements of
;;; Pascal's triangle using a recursive process.

; Pascal's triangle looks like this:

;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;; ...   ...

; ... where the numbers at the edges are all 1's, and each number inside the
; triangle is the sum of the two numbers directly above it.

; If we slant the triangle over to the left, it looks like this:

;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; ...   ...

; Then we can see that the first column will always have only 1's, 
; there are 1's across the leading diagonal (where the row number
; equals the column number), and for a number in any other position
; (i,j), it is the sum of the number directly above it, ie. (i-1,j),
; and the number above it and one over to the left, ie. (i-1,j-1).


; So the procedure for the recursive process is then straight-forward:

(define (pascal row column)
  (cond ((= column 1) 1)
        ((= column row) 1)
        (else (+ (pascal (- row 1) (- column 1))
                 (pascal (- row 1) column)))))


; And we can use this procedure to print out the next row of the triangle:

(pascal 6 1)
;> 1

(pascal 6 2)
;> 5

(pascal 6 3)
;> 10

(pascal 6 4)
;> 10

(pascal 6 5)
;> 5

(pascal 6 6)
;> 1


;; 1
;; 1 1
;; 1 2  1
;; 1 3  3  1
;; 1 4  6  4 1
;; 1 5 10 10 5 1