;;; SICP Exercise 2.56
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we improved upon the symbolic differentiation
;;; system by implementing the chain rule.


; We have the variable checking procedures, as well as the constructors 
; and selectors for sums and products in the differentiation system as
; follows:

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


; Now assume that we already have the procedures exponentiation?, 
; base, exponent and make-exponentiation. Then we simply add a new
; clause to the deriv procedure to account for the chain rule when
; we have an exponentiation expression:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))


; Now we'll represent an exponentiation symbolically as "**". So in our
; exponentiation checker, we simply check whether the first element in
; the list is the ** symbol:

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))


; For our constructor, we want to build in the rules that anything
; raised to the power of 0 is 1, and anything raised to the power
; of one is itself.
; (While we're at it, we'll also go ahead and perform the exponentiation
; if both the base and the exponent are numbers.)

(define (make-exponentiation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((and (number? b) (number? p)) (expt b p))
        (else (list '** b p))))


; Finally for our selectors, we simply return the 2nd and 3rd elements of
; that list accordingly.

(define (base e) (cadr e))

(define (exponent e) (caddr e))


; And this should now work! We'll test some examples:

(deriv '(** x 3) 'x)
;> (* 3 (** x 2))

(define test-expression '(+ (* 3 (* x y)) (* (** x 3) (** y 2))))

(deriv test-expression 'x)
;> (+ (* 3 y) (* (* 3 (** x 2)) (** y 2)))

(deriv test-expression 'y)
;> (+ (* 3 x) (* (** x 3) (* 2 y)))

(display (deriv '(** (* (* 2 (** x 2)) y) 4) 'x))
;> (* (* 4 (** (* (* 2 (** x 2)) y) 3)) (* (* 2 (* 2 x)) y))
