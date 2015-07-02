;;; SICP Exercise 2.86
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we modified our arithmetic system to handle
;;; complex numbers whose real/imag and mag/ang parts can be other
;;; types of numbers.


; To avoid overcomplicating things, we'll still suppose that complex
; numbers are always created in rectangular form (the polar form
; implementation would work the same way, but the code for this system
; is already getting pretty long and unwieldy as it is! So for the
; purposes of brevity, we won't worry about it.)

; So our numeric tower code from Exercise 2.85 is included as follows:

; Integer type:
(define (make-integer n)
  (cons 'integer n))

(define (integral-value n) n)

; Rational type:
(define (make-rational n d)
  (cons 'rational (cons n d)))

(define (numer q)
  (car q))

(define (denom q)
  (cdr q))

; Real type:
(define (make-real x)
  (cons 'real x))

(define (real-value x) x)

; Complex type:
(define (make-complex x y)
  (cons 'complex (cons x y)))

(define (real-part-r z)
  (car z))

(define (imag-part-r z)
  (cdr z))

(define (raise-integer n)
  (make-rational (integral-value n) 1))

(define (raise-rational q)
  (make-real (/ (numer q) (denom q))))

(define (raise-real x)
  (make-complex (real-value x) 0))

(define (type-tag datum)
  (cond ((number? datum) 'real)
        ((pair? datum) (car datum))
        (else (error "Bag tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

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

(put 'raise '(integer) raise-integer)
(put 'raise '(rational) raise-rational)
(put 'raise '(real) raise-real)

(define (raise num)
  (apply-generic 'raise num))

(define (tower-level type)
  ((get 'tower-level type)))

(put 'tower-level 'integer (lambda () 1))
(put 'tower-level 'rational (lambda () 2))
(put 'tower-level 'real (lambda () 3))
(put 'tower-level 'complex (lambda () 4))

(define (same-level? types)
  (cond ((null? types) true)
        ((null? (cdr types)) true)
        (else (and (= (tower-level (car types)) 
                      (tower-level (cadr types)))
                   (same-level? (cdr types))))))

(define (highest-level types)
  (apply max (map tower-level types)))

(define (raise-to-level num level)
  (if (>= (tower-level (type-tag num)) level)
      num
      (raise-to-level (raise num) level)))

(define (raise-all-to-highest-level args)
  (let ((type-tags (map type-tag args)))
    (let ((highest-tower-level (highest-level type-tags)))
      (map (lambda (arg) (raise-to-level arg highest-tower-level))
           args))))

(define (equ? x y) (apply-generic 'equ? x y))

(define (equ-integer? n1 n2)
  (= (integral-value n1) (integral-value n2)))

(define (equ-rational? q1 q2)
  (and (= (numer q1) (numer q2))
       (= (denom q1) (denom q1))))

(define (equ-real? x1 x2)
  (= (real-value x1) (real-value x2)))

(define (equ-complex? z1 z2)
  (and (= (real-part-r z1) (real-part-r z2))
       (= (imag-part-r z1) (imag-part-r z2))))

(put 'equ? '(integer integer) equ-integer?)
(put 'equ? '(rational rational) equ-rational?)
(put 'equ? '(real real) equ-real?)
(put 'equ? '(complex complex) equ-complex?)

(define (project-complex z)
  (make-real (real-part-r z)))

(define (project-real x)
  (make-integer (round x)))

(define (project-rational q)
  (make-integer (round (/ (numer q) (denom q)))))

(define (project num)
  (apply-generic 'project num))

(put 'project '(complex) project-complex)
(put 'project '(real) project-real)
(put 'project '(rational) project-rational)

(define (can-drop? num)
  (cond ((not (pair? num)) false)
        ((eq? 'integer (type-tag num)) false)
        ((equ? num (raise (project num))) true)
        (else false)))

(define (drop num)
  (if (can-drop? num)
      (drop (project num))
      num))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (if (same-level? type-tags)
        (let ((proc (get op type-tags)))
          (if proc
              (let ((result (apply proc (map contents args))))
                (if (eq? op 'raise)
                    result
                    (drop result)))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags))))      
        (apply apply-generic op (raise-all-to-highest-level args)))))


; Now for our complex number type, we need the additional selectors
; for the magnitude and angle, which are defined in terms of squares,
; square roots and arctangents. (For the polar form case, we'd also
; need to consider sines and cosines as well.)
;
; So first off, we need to define some generic procedures to perform
; those operations. We'll also bring in the usual add/sub/mul/div
; operations too:

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))
(define (arctangent x y) (apply-generic 'arctangent x y))

; Then we can define our complex number selectors 'magnitude' and
; 'angle' in terms of these operations.

(define (magnitude z)
  (square-root (add (square (real-part z))
                    (square (imag-part z)))))

(define (angle z)
  (arctangent (imag-part z) (real-part z)))

; And this will work just fine when we actually implement the needed
; operations for the integer, rational and real types (which would
; be done in the respective type packages.)

(put 'add '(integer integer)
     (lambda (m n)
       (make-integer (+ (integral-value m) (integral-value n)))))

(put 'add '(rational rational)
     (lambda (p q)
       (make-rational (+ (* (numer p) (denom q))
                         (* (numer q) (denom p)))
                      (* (denom p) (denom q)))))

(put 'add '(real real)
     (lambda (x y)
       (make-real (+ (real-value x) (real-value y)))))

(put 'real-part '(complex) (lambda (z) (real-part-r z)))

(put 'imag-part '(complex) (lambda (z) (imag-part-r z)))

(put 'square '(integer)
     (lambda (n)
       (make-integer (* (integral-value n) (integral-value n)))))

(put 'square '(rational)
     (lambda (q)
       (make-rational (* (numer q) (numer q))
                      (* (denom q) (denom q)))))

(put 'square '(real)
     (lambda (x)
       (make-real (* (real-value x) (real-value x)))))

; Note that the square-root operation isn't closed over the set
; of integers and rationals -- we may end up with a real number
; result instead, so we simply create one right off the bat.
; (Our 'drop' functionality from the previous exercise will 
; simplify the type if possible.)

(put 'square-root '(integer)
     (lambda (n)
       (make-real (sqrt (integral-value n)))))

(put 'square-root '(rational)
     (lambda (q)
       (make-real (sqrt (/ (numer q) (denom q))))))

(put 'square-root '(real)
     (lambda (x)
       (make-real (sqrt (real-value x)))))

; Similarly, the arctangent will likely be a real number in
; most cases, so we'll return only real numbers -- if we happen
; to get an integer, then it will be simplified with the 'drop'
; procedure.

(put 'arctangent '(integer integer)
     (lambda (m n)
       (make-real (atan (integral-value m) (integral-value n)))))

(put 'arctangent '(rational rational)
     (lambda (p q)
       (make-real (atan (/ (numer p) (denom p))
                        (/ (numer q) (denom q))))))

(put 'arctangent '(real real)
     (lambda (x y)
       (make-real (atan (real-value x) (real-value y)))))


; And we're done! We can test the (rectangular form) complex number
; selectors as follows.

(define c1 (make-complex (make-integer 1) (make-integer 0)))
(define c2 (make-complex (make-rational 1 2) (make-rational 1 2)))
(define c3 (make-complex (make-real 1) (make-rational 1 1)))

(magnitude c1)
;> (integer . 1)

(angle c1)
;> (integer . 0)

(imag-part c2)
;> (rational 1 . 2)

(angle c2)
;> (real . 0.7853981633974483)

(real-part c3)
;> (integer . 1)

(angle c3)
;> (real . 0.7853981633974483)
 
; The polar form, with the sine and cosine generic procedures, is 
; implemented in much the same way.