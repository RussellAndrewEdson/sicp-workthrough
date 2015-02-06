;;; SICP Exercise 2.44
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the up-split procedure used by
;;; the corner-split procedure for the picture language.

; We have the definition for the corner-split operation as follows:

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; This operation uses the right-split operation, which is defined
; in the following way:

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


; We need to implement the up-split operation, which takes in a painter and
; a number n and returns a new painter with n "splits" towered above the
; original. In fact, this will be very similar to the recursive definition
; for right-split:
;   - if n = 0, we return the original painter.
;   - otherwise, we want to return a new painter which has the original painter
;     -below- two up-splits of the painter placed -beside- each other.

; So our up-split procedure looks like this:

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


; We can actually test this, too. (If you're using the DrRacket environment 
; with Neil Van Dyke's SICP package, we have the SICP picture language package 
; by Mike Sperber and Jens Axel Soegaard available to us which brings in the
; picture primitives. See the other file, exercise2-44_test.png, to check
; out our procedures at work.) 