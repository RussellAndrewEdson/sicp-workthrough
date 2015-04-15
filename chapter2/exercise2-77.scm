;;; SICP Exercise 2.77
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we investigated the effect of evaluating the
;;; expression (magnitude z) with our full arithmetic system.

; The complex number packages for the arithmetic system are as follows.

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bag tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

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
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; So on to the actual exercise now. Suppose we have the following
; complex number data object:

(define z (make-complex-from-real-imag 3 4))
;> (complex rectangular 3 . 4)

; Then if we tried to find its magnitude with the system as it is,
; we would get the following error:

;; (magnitude z)
;;> No method for these types -- APPLY-GENERIC (magnitude (complex))

; But if we redefined the complex package and added in the lines as
; given:

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  ;; We've added these lines now!
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  'done)

; And reinstall the complex number package:
(install-complex-package)

; Then we can find the magnitude just fine.

(magnitude z)
;> 5


; This works because now we've actually defined the selectors
; for the 'two-tag' complex numbers. In our first call to
; the magnitude procedure, we were using the procedure that dispatched
; only on '(rectangular) or '(polar) data types, and so we got an error
; when it tried to get the right operation with the apply-generic
; call.
;
; But when we fix it by adding the line 
;   (put 'magnitude '(complex) magnitude)
; into the complex number package, now the magnitude procedure "knows"
; what to do when given a '(complex) data type, and the call works.


; We can trace it through using our good old substitution model:

; 1.
;;  (magnitude z)

; 2.
;;  (apply-generic 'magnitude z)

; 3.
;;  (let ((type-tags (map type-tag (list z))))
;;    (let ((proc (get 'magnitude type-tags)))
;;      (if proc
;;          (apply proc (map contents (list z)))
;;          (error 
;;           "No method for these types -- APPLY-GENERIC"
;;           (list 'magnitude type-tags)))))


; Now keeping in mind that z is defined as
;   (cons 'complex (cons 'rectangular (cons 3 4)))
; ..we have:

; 4.
;;  (let ((type-tags (map type-tag (list (cons 'complex (cons 'rectangular (cons 3 4)))))))
;;    (let ((proc (get 'magnitude type-tags)))
;;      (if proc
;;          (apply proc (map contents (list (cons 'complex (cons 'rectangular (cons 3 4))))))
;;          (error 
;;           "No method for these types -- APPLY-GENERIC"
;;           (list 'magnitude type-tags)))))

; 5.
;;  (let ((type-tags (list 'complex)))
;;    (let ((proc (get 'magnitude type-tags)))
;;      (if proc
;;          (apply proc (map contents (list (cons 'complex (cons 'rectangular (cons 3 4))))))
;;          (error 
;;           "No method for these types -- APPLY-GENERIC"
;;           (list 'magnitude type-tags)))))

; 6.
;;  (let ((proc (get 'magnitude (list 'complex))))
;;    (if proc
;;        (apply proc (map contents (list (cons 'complex (cons 'rectangular (cons 3 4))))))
;;        (error 
;;         "No method for these types -- APPLY-GENERIC"
;;         (list 'magnitude 'complex))))


; Now we retrieve the 'magnitude operation from the table,
; which returns us the version that calls 'apply-generic' again.

; 7.
;;  (let ((proc (lambda (z) (apply-generic 'magnitude z))))
;;    (if proc
;;        (apply proc (map contents (list (cons 'complex (cons 'rectangular (cons 3 4))))))
;;        (error 
;;         "No method for these types -- APPLY-GENERIC"
;;         (list 'magnitude 'complex))))

; 8.
;;  (apply (lambda (z) (apply-generic 'magnitude z)) 
;;         (map contents (list (cons 'complex (cons 'rectangular (cons 3 4))))))

; 9.
;;  (apply (lambda (z) (apply-generic 'magnitude z))
;;         (list (cons 'rectangular (cons 3 4))))

; 10.
;;  (apply-generic 'magnitude (cons 'rectangular (cons 3 4)))

; 11.
;;  (let ((type-tags (map type-tag (list (cons 'rectangular (cons 3 4))))))
;;    (let ((proc (get 'magnitude type-tags)))
;;      (if proc
;;          (apply proc (map contents (list (cons 'rectangular (cons 3 4)))))
;;          (error 
;;           "No method for these types -- APPLY-GENERIC"
;;           (list 'magnitude type-tags)))))

; 12.
;;  (let ((type-tags (list 'rectangular)))
;;    (let ((proc (get 'magnitude type-tags)))
;;      (if proc
;;          (apply proc (map contents (list (cons 'rectangular (cons 3 4)))))
;;          (error 
;;           "No method for these types -- APPLY-GENERIC"
;;           (list 'magnitude type-tags)))))

; 13.
;;  (let ((proc (get 'magnitude (list 'rectangular))))
;;    (if proc
;;        (apply proc (map contents (list (cons 'rectangular (cons 3 4)))))
;;        (error 
;;         "No method for these types -- APPLY-GENERIC"
;;         (list 'magnitude (list 'rectangular)))))

; 14.
;;  (let ((proc (get 'magnitude (list 'rectangular))))
;;    (if proc
;;        (apply proc (map contents (list (cons 'rectangular (cons 3 4)))))
;;        (error 
;;         "No method for these types -- APPLY-GENERIC"
;;         (list 'magnitude (list 'rectangular)))))


; And here we end up retrieving the magnitude operation defined
; for '(rectangular) data types.

; 15.
;;  (let ((proc (lambda (z) (sqrt (+ (square (car z))
;;                                   (square (cdr z)))))))
;;    (if proc
;;        (apply proc (map contents (list (cons 'rectangular (cons 3 4)))))
;;        (error 
;;         "No method for these types -- APPLY-GENERIC"
;;         (list 'magnitude (list 'rectangular)))))

; 16.
;;  (apply (lambda (z) (sqrt (+ (square (car z))
;;                              (square (cdr z)))))
;;         (map contents (list (cons 'rectangular (cons 3 4)))))

; 17.
;;  ((lambda (z) (sqrt (+ (square (car z))
;;                        (square (cdr z)))))
;;   (cons 3 4))

; 18.
;;  5


; And that's the trace of all of the procedures calling in evaluating
; the expression (magnitude z).
;
; Notice that we invoked the 'apply-generic' procedure twice: once on
; the full ('complex 'rectangular (cons 3 4)) object, and then again later
; for the ('rectangular (cons 3 4)) object. The first time we dispatch back to
; the generic magnitude procedure that calls 'apply-generic' again. But for the 
; second call we dispatch to the magnitude procedure defined in the rectangular 
; package closure, which knows how to actually calculate the result.
