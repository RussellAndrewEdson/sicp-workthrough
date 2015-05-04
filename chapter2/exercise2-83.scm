;;; SICP Exercise 2.83
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we designed some procedures to raise our
;;; number types up one level in the integer->rational->real->complex
;;; type tower.

; Suppose we have an arithmetic system with integral, rational, real and
; complex number types. For the sake of example, suppose that we have the
; types defined by the following tagged data structures with their 
; constructors and selectors:

; Integer type:
(define (make-integer n)
  (cons 'integer n))

(define (integral-value n) n)

; Rational type:
(define (make-rational n d)
  (cons 'rational (cons n d)))

(define (numerator q)
  (car q))

(define (denominator q)
  (cdr q))

; Real type:
(define (make-real x)
  (cons 'real x))

(define (real-value x) x)

; Complex type -- for simplicity, suppose we only consider the 
; x+yi rectangular form. (In reality, this is the form we would
; most easily raise a real number to anyway, so the simplification 
; is fine.)
(define (make-complex x y)
  (cons 'complex (cons x y)))

(define (real-part z)
  (car z))

(define (imag-part z)
  (cdr z))


; Now for the type tower integer->rational->real->complex, we
; can define the raising operations in the following way.

; To raise an integer up one level to a rational, we simply take 
; the integral value as the numerator, and set the denominator 
; equal to the value of the integer 1.
(define (raise-integer n)
  (make-rational (integral-value n) 1))

; Next, the easiest way to raise a rational up one level to a real
; number is to actually perform the numerator/demoninator division:
(define (raise-rational q)
  (make-real (/ (numerator q) (denominator q))))

; Finally, to raise a real number to a complex number, we simply
; pass the value of the real number in for the 'real-part', and use
; 0 for the imaginary part.
(define (raise-real x)
  (make-complex (real-value x) 0))

; And the 'complex' type is at the top of the tower, so we don't
; define a raise operation for complex types.

; Finally, we can add this 'raise' operation to the operation table
; in the same way that we have for any operation under the data-driven
; paradigm.
;
; We'll bring in the code for the operations table and an old apply-generic
; procedure:

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error 
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bag tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; Then we can simply install the raise procedures with the 
; following statements:

(put 'raise '(integer) raise-integer)
(put 'raise '(rational) raise-rational)
(put 'raise '(real) raise-real)

; And we can use the apply-generic procedure to define a generic
; 'raise' operation that will work for each of the types.

(define (raise num)
  (apply-generic 'raise num))


; Then we can use our generic procedure to raise given types of
; numbers up the tower.

(define num-1 (make-integer -2))
;> (integer . -2)

(raise num-1)
;> (rational -2 . 1)

(raise (raise num-1))
;> (real . -2)

(raise (raise (raise num-1)))
;> (complex -2 . 0)


(define num-2 (make-rational 8 3))
;> (rational 8 . 3)

(raise num-2)
;> (real . 8/3)

(raise (raise num-2))
;> (complex 8/3 . 0)


(define num-3 (make-real 3.14159))
;> (real . 3.14159)

(raise num-3)
;> (complex 3.14159 . 0)
