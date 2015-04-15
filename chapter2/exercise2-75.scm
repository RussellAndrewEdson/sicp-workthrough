;;; SICP Exercise 2.75
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented the make-from-mag-ang
;;; constructor for the complex number system using message-passing.

; This constructor looks identical to the one given in the book
; for the make-from-real-imag constructor, except we use the polar
; coordinate procedures for the operations instead.

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

; So the attributes of the complex number created with this
; constructor are accessed as follows.

(define z (make-from-mag-ang 2 3.14159265))

(z 'magnitude)
;> 2

(z 'angle)
;> 3.14159265

(z 'real-part)
;> -2.0

(z 'imag-part)
;> 7.1795860596832236e-09