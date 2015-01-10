;;; SICP Exercise 1.40
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise we wrote a procedure that could be used with the
;;; newtons-method procedure defined in the book to approximate zeros 
;;; of a cubic x^3 + ax^2 + bx + c.


; The code for newtons-method from the book was written in terms of the
; fixed-point procedure, and a derivative procedure, and a 
; 'newton transformation'. They are all included below so we can test our
; cubic procedure later:

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


; Now for our cubic function, we'll need notions of cubing and
; squaring a given number:

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))


; Then we can write our cubic procedure. Note that we take in the arguments
; a, b, c for the coefficients and return a procedure that computes the cubic
; at a given point x:

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


; We can then use the procedure together with our newtons-method procedure to
; compute the zeros of cubic functions.

(newtons-method (cubic -3 3 -1) 0.5)
;> 1.0000028488500885
;; x^3 - 3x^2 + 3x - 1 = (x-1)^3, so 1.0 is the zero.


; In particular, we also have another way to calculate cube roots here:

(define (cube-root x)
  (newtons-method (cubic 0 0 (- x)) 1.0))

(cube-root 20)
;> 2.714417616594914
;; For a comparison, my calculator gives 2.714417617.