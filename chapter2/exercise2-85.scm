;;; SICP Exercise 2.85
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we designed a 'drop' procedure to coerce
;;; our numeric types down the tower, and updated the apply-generic
;;; procedure to simplify its results.


; We'll bring in all of the numeric tower code we have so far (without
; the apply-generic procedure, which we'll define later):

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

(define (real-part z)
  (car z))

(define (imag-part z)
  (cdr z))

(define (raise-integer n)
  (make-rational (integral-value n) 1))

(define (raise-rational q)
  (make-real (/ (numer q) (denom q))))

(define (raise-real x)
  (make-complex (real-value x) 0))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
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


; Now as hinted in the book, we'll create our 'drop' procedure by defining
; a 'project' procedure that lowers a given type one level if possible 
; (ie. complex->real->rational->integer). We then use the project procedure
; on a given number, followed by the raise procedure; if we get the same
; number back afterward, then we didn't lose any information during the
; projection, and so it's safe to "drop" down to the lower level.
;
; So first, we'll need our generic equality predicate. We can pretty much
; use the same style as the one we had in Exercise 2.79:

(define (equ? x y) (apply-generic 'equ? x y))

(define (equ-integer? n1 n2)
  (= (integral-value n1) (integral-value n2)))

(define (equ-rational? q1 q2)
  (and (= (numer q1) (numer q2))
       (= (denom q1) (denom q1))))

(define (equ-real? x1 x2)
  (= (real-value x1) (real-value x2)))

(define (equ-complex? z1 z2)
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))))

(put 'equ? '(integer integer) equ-integer?)
(put 'equ? '(rational rational) equ-rational?)
(put 'equ? '(real real) equ-real?)
(put 'equ? '(complex complex) equ-complex?)


; Now we need to decide how we'll project the different number types
; down the tower.

; As given in the book, we can project a complex number to a real number
; by dropping the imaginary part:

(define (project-complex z)
  (make-real (real-part z)))

; Now as for projecting a real number, there's no easy way to project a
; real number to a rational number. (Well, this isn't technically true:
; since we're dealing with finite precision arithmetic, a sure-fire way
; to convert a "real number" to a rational one would be to multiply by a
; power of 10 sufficient to get rid of any non-zero decimal point values,
; and then construct a rational using the remaining integer as the
; numerator and the power of 10 as the denominator. The problem with this
; though is that it truly is sure-fire -- no information could ever be
; lost in this conversion, and so under this system every real number would
; be safely dropped to a rational, which kind of defeats the purpose of having
; the real numbers and the rational numbers as separate levels in our numeric 
; tower. [We'd never get a real number as a result from apply-generic, since 
; we could always drop it to a rational.])
;
; So instead, we'll just go ahead and project a given real number straight to
; an integer. The book suggests using the round procedure for this task:

(define (project-real x)
  (make-integer (round x)))

; And for a given rational, we can use this same idea: We'll perform the
; implied division, and then round the result.

(define (project-rational q)
  (make-integer (round (/ (numer q) (denom q)))))

; So we can add these projection operations to the table and define the
; generic project procedure like this:

(define (project num)
  (apply-generic 'project num))

(put 'project '(complex) project-complex)
(put 'project '(real) project-real)
(put 'project '(rational) project-rational)


; With those procedures defined, we can write our drop procedure.
; The procedure simply takes a number, and recursively lowers
; it one level as long as the projection doesn't lose information.
;
; We define the ancillary 'can-drop?' procedure to determine when
; this is the case. Note that we immediately return false if given
; an integer, or if we don't have one of our numeric types (which we
; can check with the 'pair?' procedure -- this allows us to use the
; 'equ?' operation with apply-generic, without any problems.)

(define (can-drop? num)
  (cond ((not (pair? num)) false)
        ((eq? 'integer (type-tag num)) false)
        ((equ? num (raise (project num))) true)
        (else false)))

(define (drop num)
  (if (can-drop? num)
      (drop (project num))
      num))


; And that's it! Then for our apply-generic procedure, we simply
; drop the result after we've calculated it (possibly with successive
; raising.) 
;
; One thing that we have to watch out for here is that we can loop 
; forever if we attempt to drop on a raise operation. So we
; should check for those first.

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


; We can test this in a similar way to how we tested the code at the end
; of Exercise 2.84:

(define (add x y z)
  (apply-generic 'add x y z))

(put 'add '(real real real) 
     (lambda (x y z)
       (make-real (+ (real-value x)
                     (real-value y)
                     (real-value z)))))

(put 'add '(complex complex complex)
     (lambda (x y z)
       (make-complex 
        (+ (real-part x) (real-part y) (real-part z))
        (+ (imag-part x) (imag-part y) (imag-part z)))))

(define num-1 (make-integer 1))
(define num-2 (make-rational 1 2))
(define num-3 (make-real 0.5))
(define num-4 (make-real 3.1415))
(define num-5 (make-complex 3 0))

(add num-1 num-2 num-3)
;> (integer . -1.0)

(add num-2 num-3 num-5)
;> (integer . 4.0)

(add num-2 num-4 num-5)
;> (real . 6.641500000000001)