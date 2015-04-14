;;; SICP Exercise 2.73
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we reconsidered the symbolic differentiation
;;; program from Exercises 2.56 - 2.58 and looked at it in terms of
;;; dispatching on a 'type' of derivative expression under the
;;; data-directed programming style.


; Our original 'deriv' procedure for the symbolic differentiation 
; program looked like this (I've also included all of the other
; procedures for the symbolic differentiation system below too):

;;  (define (deriv exp var)
;;    (cond ((number? exp) 0)
;;          ((variable? exp) (if (same-variable? exp var) 1 0))
;;          ((sum? exp)
;;           (make-sum (deriv (addend exp) var)
;;                     (deriv (augend exp) var)))
;;          ((product? exp)
;;           (make-sum
;;            (make-product (multiplier exp)
;;                          (deriv (multiplicand exp) var))
;;            (make-product (deriv (multiplier exp) var)
;;                          (multiplicand exp))))
;;          (else (error "unknown expression type -- DERIV" exp))))

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

;;  (define (sum? x)
;;    (and (pair? x) (eq? (car x) '+)))

;;  (define (addend s) (cadr s))

;;  (define (augend s) (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;  (define (product? x)
;;    (and (pair? x) (eq? (car x) '*)))

;;  (define (multiplier p) (cadr p))

;;  (define (multiplicand p) (caddr p))


; By rewriting it in the data-directed style (using the operator symbols
; as the type tags), we are given this new representation for deriv:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))


; a)
; Here we've modified the conditional expression to perform a
; table lookup for the derivative procedure to apply (when we don't
; have a number or a variable).

; That is, we have a "table" that sort of looks like this:
;
;         | +           | *           |   ..other types ..
; --------------------------------------------------------------
;  deriv  | (deriv, +)  | (deriv, *)  |  ...
; --------------------------------------------------------------
;
; ... where the elements of the table (deriv, op) are the procedures
; that perform the relevant differentiation rule for the given type.

; Now note that the way the code has been set up, we aren't really
; "tagging" the data so much as simply dispatching on the car of the
; expression. In prefix notation, our expressions are lists '(op a1 a2 ...)
; where the car is the operation op, and cdr will return us a list of 
; the operands.
;
; So since this is the case, we can't assimilate the code for dealing
; with standalone numbers or variables into the table, as we don't have
; a list structure of the form '(op a1 a2) with these!

; (Of course, if we were to tag the data with its expression type we
; could assimilate them into the data table. We would deal in terms of
; tagged data that looked like (type expr), where we would dispatch
; on 'type' in order to figure out how to compute the derivative of expr.
; Then we could happily have types like 'number' and 'variable'.
; 
; But then all levels of our input and output expressions to the 'deriv' 
; procedure would have to be tagged too, since the procedure relies on 
; having that information handy during its recursive calls. So whereas we 
; could call the above 'deriv' procedure with an expression like (* 2 x) 
; and get back 2 as a result, with our hypothetical deriv procedure we would 
; have to call it with an expression like (product ((number 2) (variable x)))
; and we'd get back (number 2), which doesn't look anywhere near as nice 
; (not to mention that we'd have to rewrite all of the predicate, constructor
; and selector procedures all over again to work with this new data form.))


; b)
; So as hinted at in the previous paragraph, our 'deriv' procedure can use
; the existing selectors and constructors defined above to carry out the work.

; Now we'll need the 'get' and 'put' procedures that are given in the book
; later in chapter 3 (so we can test things out!). So here they are; we won't
; worry about how the procedures work at this point.

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))


; So with that done, we can simply work with the operation table
; in terms of the 'put' and 'get' procedures.

; So our sum and product derivative procedures are pretty much the same 
; as they were before (taking the relevant expressions from the conditional
; that we used to have), and we simply wrap them in 'install package'
; procedures that install them into the operation table.
;
; A few changes worth noting though:
;  1. Since we've isolated the operands in the call, our selectors are
;     slightly different. Instead of taking the cadr and caddr, we just
;     take the car and the cadr instead (since we don't need to skip over
;     the operator in the list any more.)
;
;  2. Also, our new derivative procedures need to take 'var' (the variable
;     we're differentiating with respect to) as the second argument.

(define (install-sum-derivative-package)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (sum-derivative exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  
  (put 'deriv '+ sum-derivative))

(define (install-product-derivative-package)
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (product-derivative exp var)
    (make-sum 
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))
  
  (put 'deriv '* product-derivative))


; And that's it! We can install the packages and test them as follows:

(install-sum-derivative-package)
;> 'ok

(install-product-derivative-package)
;> 'ok

(deriv '(+ (* 2 (* x y)) (* 3 x)) 'x)
;> (+ (* 2 y) 3)

(deriv '(* x (+ y z)) 'x)
;> (+ y z)

; So that's good; the program appears to be working with the new
; data-driven style.


; c)
; So to see how easy it is to just add new types now, let's add that
; exponential type that we had back in Exercise 2.56. This is exactly
; as simple as writing another 'install package' procedure.

; Recall that we represented exponentials with the symbol '**. We
; can use the exact same constructor that we had last time, too:

(define (make-exponentiation b p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) b)
        ((and (number? b) (number? p)) (expt b p))
        (else (list '** b p))))

(define (install-exponential-derivative-package)
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (exponential-derivative exp var)
    (make-product (make-product (exponent exp)
                                (make-exponentiation (base exp)
                                                     (- (exponent exp) 1)))
                  (deriv (base exp) var)))
  
  (put 'deriv '** exponential-derivative))


; This one should work straight-away too!

(install-exponential-derivative-package)
;> 'ok

(deriv '(** x 2) 'x)
;> (* 2 x)


; d)
; Suppose we swapped the order of the table indexing, so instead of:
;   (get 'deriv (operator exp))
;
; we have:
;   (get (operator exp) 'deriv))
;
; Then this basically flips our operations table, so it looks like this now:
;
;     | deriv   
; -----------------
;  +  | (+, deriv)
; -----------------
;  *  | (*, deriv)
; -----------------
;    ...    ...

; So to get the program working again, all we'd need to do is 
; swap the order for the table indices in our calls to 'put' when
; we install anything.
;
; So for example,
;    (put 'deriv '** exponential-derivative)
; should now be
;    (put '** 'deriv exponential-derivative)
;
; And this is fine (the choice of "operations on the rows, types
; across the columns" was an arbitrary one, after all. We're free to
; swap that around all we like.)
