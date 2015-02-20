;;; SICP Exercise 2.58
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to modify the symbolic differentiation
;;; system to work with infix (not prefix) operators.


; To keep things simple, we'll bring in our old system (before we added
; exponentiation support in Exercise 2.56). 
; So here's the base predicates for the variables/number checks:

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; We'll define the 'deriv' procedure separately in parts a) and b)
; so that we don't have any name clashes between them.


; a)
; We'll use this deriv procedure for part a):

(define (deriv-a exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-a exp)
         (make-sum-a (deriv-a (addend-a exp) var)
                     (deriv-a (augend-a exp) var)))
        ((product?-a exp)
         (make-sum-a
          (make-product-a (multiplier-a exp)
                          (deriv-a (multiplicand-a exp) var))
          (make-product-a (deriv-a (multiplier-a exp) var)
                          (multiplicand-a exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; For simplicity, we'll assume that + and * take two arguments
; here.


; Now since we have that a sum (+) takes two arguments, then we
; simply check whether the 2nd element of the expression is the
; symbol '+ for the infix notation:

(define (sum?-a x)
  (and (pair? x) (eq? (cadr x) '+)))

; Similarly, a product expression will have the symbol '* as
; its second element:

(define (product?-a x)
  (and (pair? x) (eq? (cadr x) '*)))


; Similarly, the addend/multiplier will be the very first element
; of the expression this time around:

(define (addend-a s) (car s))

(define (multiplier-a p) (car p))


; And the augend/multiplicand will be the third element of the 
; expression:

(define (augend-a s) (caddr s))

(define (multiplicand-a p) (caddr p))


; Then our constructors simply position the '+, '* symbols in the
; middle of the sum/product expression.

(define (make-sum-a a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
  
(define (make-product-a m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


; Then this should all work to enable us to differentiate infix form
; expressions:

(deriv-a '(x + (3 * (x + (y + 2)))) 'x)
;> 4

(deriv-a '((x * y) + (3 * (x * x))) 'x)
;> (y + (3 * (x + x)))


; b)
; We'll use this deriv procedure for part b).

(define (deriv-b exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum?-b exp)
         (make-sum-b (deriv-b (addend-b exp) var)
                     (deriv-b (augend-b exp) var)))
        ((product?-b exp)
         (make-sum-b
          (make-product-b (multiplier-b exp)
                          (deriv-b (multiplicand-b exp) var))
          (make-product-b (deriv-b (multiplier-b exp) var)
                          (multiplicand-b exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

; This time we want to allow for standard algebraic notation, where we
; drop unnecessary parentheses and always multiply before we add.


; Now for our predicates, notice that we want to do multiplications -first-.
; So given an expression like (x + 3 * (x + y + 2)), we want to consider this
; as a sum, and not a product.
;
; So basically, we want to scan across the top-level of the expression list,
; and if we ever find a '+ symbol, it's a sum. Only if we don't find a '+ will
; we then check for a product with the '* symbol.
;
; As to how we do this, recall that 'memq' procedure that we saw in the book
; quite recently! This will work to allow us to check for the existence of
; those '+, '* symbols in the list.

; So we can define our sum? predicate like this:

(define (sum?-b x)
  (if (memq '+ x)
      true
      false))

; And then we can define our product? predicate by first making sure that
; we don't actually have a sum.

(define (product?-b x)
  (and (not (sum?-b x))
       (if (memq '* x)
           true
           false)))


; Next, for our addend/multiplier selectors, we want to basically return
; everything that appears before the first occurrence of the '+ symbol in
; the top-level of the list. (Basically the "reverse" of what the memq 
; procedure does.)
; 
; We can write this similar to the definition for the memq procedure 
; from the book (except we'll use an iterative process to build up the
; returned list):

(define (upto item x)
  (define (iter rest result)
    (cond ((null? rest) result)
          ((eq? item (car rest)) result)
          (else (iter (cdr rest) (append result (list (car rest)))))))
  (iter x nil))
        
; We can quickly test this to see that it works the way we want it to:

(upto '+ '((3 * 4 + 5) * 7 + 2 * (4 + 4)))
;> ((3 * 4 + 5) * 7)

; Perfect! Now we simply write our addend/multiplier procedures in terms
; of our 'upto' procedure. But we have an additional check: if we ever
; have a list with only one element, we'll just return that element by
; itself.

(define (addend-b s) 
  (let ((terms (upto '+ s)))
    (if (= (length terms) 1)
        (car terms)
        terms)))

(define (multiplier-b p) 
  (let ((terms (upto '* p)))
    (if (= (length terms) 1)
        (car terms)
        terms)))


; Similarly for our augend/multiplicand procedures, we want to return
; everything -after- the first occurrence of the symbol. The memq procedure
; already pretty much does this for us, but for readability we can even
; define an 'after' procedure to work similar to our 'upto' one:

(define (after item x)
  (let ((memq-result (memq item x)))
    (if memq-result
        (cdr memq-result)
        nil)))

; Here we've made it so that if memq returns false (ie. the item doesn't
; exist in the list), we'll just return nil.

; Again, this one should also work properly right off the bat:

(after '+ '((3 * 4 + 5) * 7 + 2 * (4 + 4)))
;> (2 * (4 + 4))

; So now our augend/multiplicand procedures are as follows (including 
; the check for a lone element in a list):

(define (augend-b s)
  (let ((terms (after '+ s)))
    (if (= (length terms) 1)
        (car terms)
        terms)))

(define (multiplicand-b p)
  (let ((terms (after '* p)))
    (if (= (length terms) 1)
        (car terms)
        terms)))


; And finally, our make-sum and make-product constructors don't actually
; have to be modified at all. We'll be able to handle standard algebraic
; notation expressions just fine already; we just won't ever output them
; as a result of the differentiation.

(define (make-sum-b a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))
  
(define (make-product-b m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))


; So putting it all together, we should be able to take the derivative
; of some standard algebraic expressions!

(deriv-b '(x + 3 * (x + y + 2)) 'x)
;> 4

(deriv-b '(3 * x * y + 2 * (x + y)) 'x)
;> ((3 * y) + 2)