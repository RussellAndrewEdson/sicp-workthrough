;;; SICP Exercise 2.88
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise, we extended the polynomial system to include
;;; subtraction of polynomials.

; Our arithmetic system is as follows.Now as per the hint, we can 
; define a generic negation operation, and include the implementations 
; for the rational and scheme numbers here:

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (negate x) (apply-generic 'negate x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error 
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

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

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bag tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))


;; Rational number package
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))  
  (define (equ-rat? x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (=zero-rat? x)
    (and (= 0 (numer x))
         (not (= 0 (denom x)))))
  
  ;;==================================================================
  ; We define the negation procedure for the rational numbers:
  
  (define (negate-rat x)
    (make-rat (- (numer x)) (denom x)))
  ;;==================================================================
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ-rat?)
  (put '=zero? '(rational) =zero-rat?)
  
  ;;==================================================================
  ; And we make sure to add our negate implementation to the
  ; operation table:
  
  (put 'negate '(rational) negate-rat)
  ;;==================================================================
  
  'done)

(install-rational-package)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Scheme number package
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) zero?)
  
  ;;==================================================================
  ; Here we define the negation procedure for scheme numbers.
  
  (put 'negate '(scheme-number)
       (lambda (x) (tag (- x))))
  ;;==================================================================
  
  'done)

(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


; And then in our polynomial package, we can define the subtraction
; procedure in terms of the negation operation; ie. to perform the
; subtraction p - q, we will instead add the negation of q, ie.
; p + (-q).

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  
  ;;==================================================================
  ; Now first we need to define what it means to negate a
  ; polynomial. We'll negate a polynomial by negating all of
  ; the coefficients in the term-list (which will allow us to
  ; use rational/polynomial/scheme-number coefficients as before.)
  ; 
  ; So here we'll define a procedure to negate a single term.
  
  (define (negate-term term) 
    (make-term (order term)
               (negate (coeff term))))
  ;;==================================================================
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  ;;==================================================================
  ; Here we want to negate an entire term list. So using our
  ; negate-term procedure, we'll check for the empty term list first 
  ; as always, and then if we -do- have terms, we'll cdr down the 
  ; term list, negating each one.
  
  (define (negate-terms L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (adjoin-term
         (negate-term (first-term L))
         (negate-terms (rest-terms L)))))
  
  ; Then with this negation procedure, we can define the 
  ; subtraction procedure for the term lists in terms of 
  ; the addition one.
  
  (define (sub-terms L1 L2)
    (add-terms L1 (negate-terms L2)))
  ;;==================================================================
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  ;;==================================================================
  ; Similarly, here we define the negation procedure for the
  ; polynomial using our term list procedures:
  
  (define (negate-poly p)
    (make-poly (variable p) 
               (negate-terms (term-list p))))
  
  ; And then we can define subtraction in terms of addition and
  ; negation as follows. We also check to make sure that our
  ; polynomials have the same variable in -this- procedure. We
  ; could have left the check out: the addition procedure does
  ; the exact same check. But this way our error is a little more
  ; precise and helpful (instead of referring us to the add-poly
  ; procedure, we'll be referred to the sub-poly one instead.)
  
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (add-poly p1 (negate-poly p2))
        (error "Polys not in same var -- SUB-POLY"
               (list p1 p2))))
  ;;================================================================== 
    
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (=zero-all-terms? L)
    (if (empty-termlist? L)
        true
        (and (=zero? (coeff (first-term L)))
             (=zero-all-terms? (rest-terms L)))))  
  (define (=zero-poly? p)
    (=zero-all-terms? (term-list p)))
  
  ;; interface to the rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial) =zero-poly?)
  
  ;;==================================================================
  ; Finally, we'll add our negation and subtraction procedures
  ; to the operation table.
  
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  ;;==================================================================
  
  'done)

(install-polynomial-package)
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


; And our arithmetic system should now be able to negate and
; subtract polynomials!

; We'll test it with a few simple cases:

(define p1 (make-polynomial 'x '((2 3) (1 -5) (0 -4))))
(define p2 (make-polynomial 'x '((3 1) (1 7) (0 2))))
(define p3 (make-polynomial 'y '((2 2) (0 -1))))
(define p4 (make-polynomial 'y '((1 7))))

(negate p1)
;> (polynomial x (2 -3) (1 5) (0 4))

(negate p2)
;> (polynomial x (3 -1) (1 -7) (0 -2))

(sub p1 p2)
;> (polynomial x (3 -1) (2 3) (1 -12) (0 -6))

(negate p3)
;> (polynomial y (2 -2) (0 1))

(sub (make-polynomial 'x (list (list 2 p4) (list 1 -1))) 
     (make-polynomial 'x (list (list 2 p3) (list 0 8))))
;> (polynomial x (2 (polynomial y (2 -2) (1 7) (0 1))) (1 -1) (0 -8))
