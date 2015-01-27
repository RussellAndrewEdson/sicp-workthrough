;;; SICP Exercise 2.27
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we modified our reverse procedure from
;;; Exercise 2.18 so that it "deep-reverses" all sublists too.

; Our original reverse procedure was as follows:

;;  (define (reverse items)
;;    (define (reverse-iter rest-of-items reverse-list)
;;      (if (null? rest-of-items)
;;          reverse-list
;;          (reverse-iter (cdr rest-of-items)
;;                        (cons (car rest-of-items) reverse-list))))
;;    (reverse-iter items nil))


; So for our new deep-reverse procedure, we want to essentially
; do the same thing as in our reverse procedure above. Except we
; want another check when we go to construct the list in the iteration
; step: if we're adding on a list, we want to reverse that list first 
; before we add it.

; So our procedure becomes:

(define (deep-reverse items)
  (define (reverse-iter rest-of-items reverse-list)
    (if (null? rest-of-items)
        reverse-list
        (reverse-iter (cdr rest-of-items)
                      (cons (if (pair? (car rest-of-items))
                                (reverse-iter (car rest-of-items) nil)
                                (car rest-of-items))
                            reverse-list))))
  (reverse-iter items nil))
                                

; We can test this with the given example:

(define x (list (list 1 2) (list 3 4)))

(reverse x)
;> {mcons {mcons 3 {mcons 4 '()}} {mcons {mcons 1 {mcons 2 '()}} '()}}
; ie. ((3 4) (1 2))

(deep-reverse x)
;> {mcons {mcons 4 {mcons 3 '()}} {mcons {mcons 2 {mcons 1 '()}} '()}}
; ie. ((4 3) (2 1))