;;; SICP Exercise 3.2
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we constructed a procedure to keep track
;;; of the number of times a given procedure has been called.


; Here we want a procedure 'make-monitored' that takes a procedure
; f of one argument, and wraps it in an environment with a counter
; that keeps track of the number of times that f has been called.

; So we will define a local variable 'count' in the procedure
; that keeps track of the number of times we call f, and 
; manipulate this value in some internal procedures 'how-many-calls?',
; 'reset-count' and 'call-f'.
;
; Finally, we return a dispatch procedure that takes in a value.
; If the value is one of the symbols 'how-many-calls? or 'reset-count,
; we call those procedures. Otherwise, we pass the input to our
; 'call-f' procedure, which increments the counter and passes the
; input to f, returning the result.
;
; Note that here we've defined our 'reset-count' procedure to always
; return the new count value (0). This is just so that it always
; has a well-defined return value (since the set! expression doesn't
; have an implementation-independent return value, as stated in one
; of the book's footnotes.)

(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?) count)
    (define (reset-count)
      (begin (set! count 0)
             count))
    (define (call-f input)
      (begin (set! count (+ count 1))
             (f input)))
    
    ;; We perform a different operation based on the input given.
    (define (dispatch input)
      (cond ((eq? input 'how-many-calls?) (how-many-calls?))
            ((eq? input 'reset-count) (reset-count))
            (else (call-f input))))
    dispatch))
            

; And we can use our monitor procedure as follows:

(define s (make-monitored sqrt))

(s 100)
;> 10

(s 'how-many-calls?)
;> 1

(s 4)
;> 2

(s 144)
;> 12

(s 'how-many-calls?)
;> 3

(s 'reset-count)
;> 0

(s '25)
;> 5

(s 'how-many-calls?)
;> 1