;;; SICP Exercise 2.45
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we defined a general 'split' procedure that
;;; implements the general splitting operation used by the right-split
;;; and up-split procedures.

; The right-split and up-split procedures were previously defined as follows:

;;  (define (right-split painter n)
;;    (if (= n 0)
;;        painter
;;        (let ((smaller (right-split painter (- n 1))))
;;          (beside painter (below smaller smaller)))))

;;  (define (up-split painter n)
;;    (if (= n 0)
;;        painter
;;        (let ((smaller (up-split painter (- n 1))))
;;          (below painter (beside smaller smaller)))))

; Looking at them side-by-side, we can easily see the common structure
; between the two procedures. 


; Now we want our split procedure to work as follows. It should take two
; arguments, and return a procedure that takes a painter and a number n,
; so that the above splitting procedures can be defined as follows:

;;  (define right-split (split beside below))
;;  (define up-split (split below beside))

; If we look back up at the way the old procedure were defined, then we 
; see that we want the first argument to define the transformation that
; separates the "base" of the picture from the part that grows recursively, 
; and the second argument defines how we split for that recursive "tower" 
; that build up (or to the right, etc) from the base.

; So we can take the common structure from the old procedures, and write
; the general split procedure as follows:

(define (split base tower)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split base tower) painter (- n 1))))
          (base painter (tower smaller smaller))))))


; As always, we can test this to make sure that our new right-split and
; up-split procedures work as they did before. (see exercise2-45_test.png.)