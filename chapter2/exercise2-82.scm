;;; SICP Exercise 2.82
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we generalised the apply-generic procedure
;;; to handle multiple argument coercion.


; Our previous apply-generic procedure (from Exercise 2.81) was as 
; follows:

;;  (define (apply-generic op . args)
;;    (let ((type-tags (map type-tag args)))
;;      (let ((proc (get op type-tags)))
;;        (if proc
;;            (apply proc (map contents args))
;;            (if (= (length args) 2)
;;                (let ((type1 (car type-tags))
;;                      (type2 (cadr type-tags))
;;                      (a1 (car args))
;;                      (a2 (cadr args)))
;;                  (if (not (eq? type1 type2))
;;                      (let ((t1->t2 (get-coercion type1 type2))
;;                            (t2->t1 (get-coercion type2 type1)))
;;                        (cond (t1->t2
;;                               (apply-generic op (t1->t2 a1) a2))
;;                              (t2->t1
;;                               (apply-generic op a1 (t2->t1 a2)))
;;                              (else
;;                               (error "No method for these types"
;;                                      (list op type-tags)))))
;;                      (error "No method for these types"
;;                             (list op type-tags))))
;;                (error "No method for these types"
;;                       (list op type-tags)))))))


; Now as suggested in the book, one way we could implement multiple-argument
; coercion would be to try to coerce all of the arguments to the first type,
; then if that doesn't work, try to coerce them to the second type, and so on.
;
; We can do this by defining an iteration procedure that moves down the list
; of possible types. For each one, we'll see if we can coerce all of the
; arguments to that type -- if so, we'll perform the coercion, and then perform
; the operation. If not, we'll keep moving down the list. If we reach the end
; of the list without finding a possible coercion, we'll return an error.
;
; So the top-level algorithm is:
;   1. Try to find an applicative procedure for the given types.
;   2. If no procedure exists, check that we have differing types (if we don't,
;      then coercion won't work).
;   3. Coerce the types and try again.
;
; So we might write our 'general' apply-generic procedure as follows (I've
; included the 'accumulate' procedure here too: we'll use it to determine
; when we can coerce all of the types.)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (apply-generic op . args)
  (define (coerce-all-to-type? type)
    (accumulate (lambda (x y) (and x y))
                true
                (map (lambda (other-type)
                       (if (eq? type other-type)
                           true
                           (get-coercion other-type type)))
                     (map type-tag args))))
  (define (all-same-type? types)
    (let ((first-type (car types)))
      (accumulate (lambda (x y) (and x y))
                  true
                  (map (lambda (other-type) (eq? first-type other-type))
                       (cdr types)))))                
  (define (try-coercion types)
    (cond ((null? types) (error "No operation possible for the given types."))
          ((coerce-all-to-type? (car types))
           (let ((new-type (car types)))
             (apply apply-generic 
                    (cons op 
                          (map (lambda (arg)
                                 (if (eq? (type-tag arg) new-type)
                                     arg
                                     ((get-coercion (type-tag arg) new-type)
                                      arg)))
                               args)))))
          (else (try-coercion (cdr types)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (cond (proc (apply proc (map contents args)))
            ((not (all-same-type? type-tags))
             (try-coercion type-tags))
            (else (error "No operation possible for the given types."))))))


; Now let's see this procedure in action! We'll bring in some of the arithmetic
; system code, but we'll only worry about the addition operation -- which
; we'll extend to work for three arguments. We'll bring in the scheme-number
; and the complex number packages so we can see some type coercion happening:

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

(define coercion-table (make-table))
(define get-coercion (coercion-table 'lookup-proc))
(define put-coercion (coercion-table 'insert-proc!))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number scheme-number)
       (lambda (x y z) (tag (+ x y z))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (x y) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)
(install-polar-package)
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;; internal procedures
  (define (add-complex z1 z2 z3)
    (make-from-real-imag (+ (real-part z1) (real-part z2) (real-part z3))
                         (+ (imag-part z1) (imag-part z2) (imag-part z3))))
  
  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex complex)
       (lambda (z1 z2 z3) (tag (add-complex z1 z2 z3))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y z) (apply-generic 'add x y z))
(define (sub x y z) (apply-generic 'sub x y z))


; So we'll define a coercion procedure to transform a scheme number
; into its complex number equivalent:

(define (scheme-number->complex n)
  (make-complex-from-real-imag n 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)


; And then our addition of different types should work!

(add 1 2.34 (make-complex-from-real-imag 5 6))
;> (complex rectangular 8.34 . 6)


; However as the book states, this strategy is not sufficiently general.
; Suppose for example that we have the following multiplication operation
; defined along with the following coercion (and that's -all-):

;;  (put 'mul '(complex complex scheme-number) ...)

;;  (define (rational->complex q) ...)

;;  (mul ((make-complex-from-real-imag 1 2) 
;;        (make-rational 3 4)
;;        5.67))

; Admittedly it's a bit contrived, but we can see straight away that
; we can perform the operation if we coerce that rational into a complex
; number.
;
; And yet, using our above strategy, we'll never even try this. We'll
; try to coerce all of the arguments into complex numbers, then try
; to coerce everything into a rational, and then try to coerce everything
; into a scheme-number, and then give up and declare the operation impossible.

; (Of course, this is a non-trivial problem to solve. The naive answer would
; be to modify the apply-generic procedure even more so that we try to
; coerce the arguments into every possible one of the n^n permutations, which
; would then make our arithmetic system prohibitively inefficient.)