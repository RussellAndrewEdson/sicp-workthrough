;;; SICP Exercise 2.57
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we extended the differentiation program to handle
;;; sums and products of arbitrary numbers of terms.


; We want to extend the symbolic differentiation system by only changing the
; representation for the sums and products. In fact, we'll only be changing
; the selectors here: the rest of the code should be unchanged from 
; Exercise 2.56:

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

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (make-exponentiation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((and (number? b) (number? p)) (expt b p))
        (else (list '** b p))))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

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


; Now the easiest way to actually modify the program to work with 
; arbitrary length sums and products is to simply modify the augend and
; multiplicand procedures. Instead of them just returning the 3rd element
; of the summation/product expression, we'll have them return the entire
; rest of the sum/product instead (which we'll reutrn as a sum/product by
; cons-ing a '+ or '* to the front as required.)

; That is, we'll first check whether we have more than 2 terms in the sum
; (we can assume that we have at least two) -- ie. if the cdddr is nil.
; If so, we just return the caddr as before; otherwise, we'll sum together
; the rest of the terms.

(define (augend s) 
  (cond ((null? (cdddr s)) (caddr s))
        (else (cons '+ (cddr s)))))


; Similarly for the product case with the multiplicand selector, except
; we're multiplying the terms together this time.

(define (multiplicand p)
  (cond ((null? (cdddr p)) (caddr p))
        (else (cons '* (cddr p)))))


; Then our deriv procedure should work to handle sums and products with
; more terms:

(deriv '(* x y (+ x 3)) 'x)
;> (+ (* x y) (* y (+ x 3)))

(deriv '(+ x (* x 7) (* x (** y 3) 7) 5) 'x)
;> (+ 1 (+ 7 (* (** y 3) 7)))


; Note from the latter example that the output still nests the sums and
; products. To change this, we'd have to modify make-sum and make-product
; to handle arbitrary term arguments, which is a lot harder -- particularly
; without changing the deriv procedure.
;
; (One way to do it would be to bring in our old filter/accumulate procedures
; and change the constructors to work in terms of lists of terms. However we'd
; then need to either modify the 'deriv' procedure to work with this new
; representation -- which completely defeats the point of the abstraction
; barrier -- or we'd need to use the dotted-notation for the arbitrary number
; of parameters to a procedure, which brings its own set of weird issues.)