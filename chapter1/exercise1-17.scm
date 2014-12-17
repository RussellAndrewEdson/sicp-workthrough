;;; SICP Exercise 1.17
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to define a fast-mult procedure
;;; in terms of repeated addition, in a way analogous to the fast-expt
;;; procedure we've previously seen and that also uses a logarithmic
;;; number of steps.

; We were given the following multiplication procedure that generates
; a recursive process (I've changed the name to 'mult' to avoid a
; name clash with the existing '*' procedure.)

(define (mult a b)
  (if (= b 0)
      0
      (+ a (mult a (- b 1)))))


; Now suppose we have doubling and halving operations available to us.
; (Note that for simplicity I've defined double in terms of a 
; multiplication. Let's not worry too much about the circularity 
; of then using that procedure to define multiplication itself.)

(define (double n) (* n 2))
(define (halve n) (/ n 2))


; Using these doubling and halving procedures, we aim to develop a procedure
; similar to the fast-expt one, that will use a logarithmic number of steps to
; do the multiplication by making use of a simplification when b is even.

; Note the following truth about multiplication:
;   a * b = 2*a * (1/2)*b
;
; Using this, we can define the procedure to do the following:
;   If b is even, then a*b = 2a*(b/2).
;   If b is odd, then a*b = a + (a*(b-1)) as usual.
;   And obviously, we return 0 if b = 0.

; So our procedure is as follows:

(define (fast-mult a b)
  (cond ((= 0 b) 0)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))


; We can test the procedure on some values to see that agrees with the
; mult procedure, and also seems to work properly with 0, 1 and commutativity:

(mult 2 0)
;> 0
(fast-mult 2 0)
;> 0
(fast-mult 0 2)
;> 0

(mult 23 1)
;> 23
(fast-mult 23 1)
;> 23
(fast-mult 1 23)
;> 23

(mult 1234 4567)
;> 5635678
(fast-mult 1234 4567)
;> 5635678
(fast-mult 4567 1234)
;> 5635678

