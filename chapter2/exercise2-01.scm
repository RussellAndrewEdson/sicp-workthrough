;;; SICP Exercise 2.1
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we improved on the make-rat procedure from the
;;; book to make it handle both positive and negative arguments.

; The previous version of make-rat (from the book) is as follows:

;;  (define (make-rat n d)
;;    (let ((g (gcd n d)))
;;      (cons (/ n g) (/ d g))))


; Now we have that the gcd procedure will always return a positive number,
; so we don't have to worry about that here.

; Instead, we can simply make a procedure to check whether the rational
; number should be positive or negative, and then construct the numerator
; and denominator using the absolute value, modifying the sign of the
; numerator as necessary.

; The procedure is then as follows. (We define an xor procedure to compute
; the exclusive-or of a pair of boolean values. The procedure returns true
; if one or the other is true, but not both -- this is perfect for checking
; whether the rational number is negative, as you can see.)

(define (xor a b)
  (and (or a b)
       (not (and a b))))

(define (make-rat n d)
  (define (negative-rat? n d)
    (xor (negative? n) (negative? d)))
  (let ((g (gcd n d))
        (normalised-n (if (negative-rat? n d)
                          (- (abs n))
                          (abs n)))
        (normalised-d (abs d)))
    (cons (/ normalised-n g) (/ normalised-d g))))


; We can test this constructor using hte numer, denom and print-rat
; methods from the book:

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(print-rat (make-rat 2 4))
;> 1/2

(print-rat (make-rat -2 4))
;> -1/2

(print-rat (make-rat 2 -4))
;> -1/2

(print-rat (make-rat -2 -4))
;> 1/2
