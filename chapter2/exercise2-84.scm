;;; SICP Exercise 2.84
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used the raise operation from Exercise 2.83
;;; to modify 'apply-generic' to coerce its arguments with successive
;;; raising.

; We'll bring in the numeric tower code that we had in Exercise 2.83:

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

; Now as it says in the book for the exercise, we're going to
; need a way to test which of two types is higher in the tower.
; In particular, we want it to be "compatible" with the rest
; of the system: that is, since we're writing the system in an
; additive, data-driven style, we'll want to keep that theme
; for our tower-level determination procedure too.

; And in fact, we can actually make use of our operation table!
; Suppose we enumerate the levels of our tower with the natural
; numbers (1, 2, 3, etc.) Then we could define a procedure 
; 'tower-level' that takes in a type symbol and returns the number
; for the level of the type.
;
; But instead of having a conditional expression to check the type,
; we can instead dispatch to the operation table:

(define (tower-level type)
  ((get 'tower-level type)))

; And then whenever we've defined new types, we simply make sure
; that we define their level in the tower, too. (Of course, one 
; drawback of this method is that the onus is on us to make sure 
; we keep our levels well-defined! Things can/will get weird when
; we start trying to insert new types "in between" existing levels.
; That's not so much of a problem with the numeric tower, but if
; we were constucting a tower for a different, more complicated 
; hierarchy, we would probably want to look at using linked-lists
; or other schemes instead, which are more suited to maintaining
; a well-defined order in the case of insertions.)

(put 'tower-level 'integer (lambda () 1))
(put 'tower-level 'rational (lambda () 2))
(put 'tower-level 'real (lambda () 3))
(put 'tower-level 'complex (lambda () 4))

; So then checking whether one type is lower than the other
; type is just a matter of comparing their tower-levels.


; Then our apply-generic function can be written to coerce the
; arguments when one type is lower than the other. Note here that
; in the case of our numeric tower, we don't even need to check
; for mixed-type operations anymore; we'll simply successively
; raise the smaller types up and apply the same-type operation
; instead. (That is, we've taken the responsibility of 'coercion' 
; for a mixed-type operation away from the operations table -- we'll
; let our tower-level idea handle it instead.)

; We'll first define a procedure 'same-level?' that takes a list of
; types and returns true if they are all on the same level of the
; numeric tower (we'll say that the empty list and the list of one
; type both have all of their types on the same level):

(define (same-level? types)
  (cond ((null? types) true)
        ((null? (cdr types)) true)
        (else (and (= (tower-level (car types)) 
                      (tower-level (cadr types)))
                   (same-level? (cdr types))))))

; Next, we want a similar procedure 'highest-level' that will return
; the number for the highest-level for the given arguments. We'll
; use this to determine the level to raise everything to:

(define (highest-level types)
  (apply max (map tower-level types)))

; Next, we want a procedure that takes a (tagged) number and a given
; tower level, and successively raises the number to that level 
; (or simply returns the number if it is at a greater-or-equal level):

(define (raise-to-level num level)
  (if (>= (tower-level (type-tag num)) level)
      num
      (raise-to-level (raise num) level)))

; Then our final ancillary procedure takes the list of tagged arguments,
; and checks for any that are not on the highest level (ie. so they
; can be raised.) It then returns a new agrument list, where all
; lower-level arguments have been raised to the highest-level.

(define (raise-all-to-highest-level args)
  (let ((type-tags (map type-tag args)))
    (let ((highest-tower-level (highest-level type-tags)))
      (map (lambda (arg) (raise-to-level arg highest-tower-level))
           args))))
             
; Then we define apply-generic as follows. We'll check whether all
; of our arguments are on the same level. If they are, we'll see if
; a procedure exists to deal with them, and if so, apply it (if not,
; we'll print the usual error.)
; 
; If our arguments are on different levels though, we'll find the
; first lower-level type, raise it, and then try again.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (if (same-level? type-tags)
        (let ((proc (get op type-tags)))
          (if proc
              (apply proc (map contents args))
              (error
               "No method for these types -- APPLY-GENERIC"
               (list op type-tags))))
        (apply apply-generic op (raise-all-to-highest-level args)))))


; Now let's test this! Suppose we define a generic, three-argument
; addition procedure like this:

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

; Then we can test our successive raising coercion system
; as follows:

(define num-1 (make-integer -2))
(define num-2 (make-rational 4 5))
(define num-3 (make-real 3.14))
(define num-4 (make-complex 1 2))

(add num-1 num-2 num-3)
;> (real . 1.9400000000000002)

(add num-1 num-2 num-4)
;> (complex -1/5 . 2)