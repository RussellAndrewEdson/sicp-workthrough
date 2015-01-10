;;; SICP Exercise 1.41
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise we wrote a procedure 'double' that returns a 
;;; procedure that applies the original procedure twice (ie. composes it
;;; with itself.)


; We're given that the procedure to apply will only ever take one argument,
; so we can write our double procedure simply as follows:

(define (double f)
  (lambda (x)
    (f (f x))))

; We can test this with an increment procedure (applying it twice should +2
; to the given argument.

(define (inc x)
  (+ x 1))

((double inc) 3)
;> 5

((double inc) 7)
;> 9


; Now what is the value of the expression (((double (double double)) inc) 5) ?
; We can use the substitution model to figure this out.
; (Alternatively, you could save yourself the time and just run it, since the
; working is kind of long and tedious!)

; Note that we can write double as (lambda (f) (lambda (x) (f (f x)))).
; We'll expand this expression all the way out and see what happens:

;; (((double (double double)) inc) 5)

;; (((double ((lambda (f) (lambda (x) (f (f x))))
;;            (lambda (f) (lambda (x) (f (f x)))))) inc) 5)

;; (((double (lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                        ((lambda (f) (lambda (x) (f (f x)))) x)))) inc) 5)

;; ((((lambda (f) (lambda (x) (f (f x))))
;;    (lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                 ((lambda (f) (lambda (x) (f (f x)))) x)))) inc) 5)

;; (((lambda (x) 
;;     ((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                   ((lambda (f) (lambda (x) (f (f x)))) x)))
;;      ((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                    ((lambda (f) (lambda (x) (f (f x)))) x))) x))) inc) 5)

;; (((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                ((lambda (f) (lambda (x) (f (f x)))) x)))
;;   ((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                 ((lambda (f) (lambda (x) (f (f x)))) x))) inc)) 5)

;; (((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                ((lambda (f) (lambda (x) (f (f x)))) x)))
;;   ((lambda (f) (lambda (x) (f (f x))))
;;    ((lambda (f) (lambda (x) (f (f x)))) inc))) 5)

;; (((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                ((lambda (f) (lambda (x) (f (f x)))) x)))
;;   ((lambda (f) (lambda (x) (f (f x))))
;;    (lambda (x) (inc (inc x))))) 5)

;; (((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                ((lambda (f) (lambda (x) (f (f x)))) x)))
;;   (lambda (x) 
;;     ((lambda (x) (inc (inc x)))
;;      ((lambda (x) (inc (inc x))) x)))) 5)

;; (((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                ((lambda (f) (lambda (x) (f (f x)))) x)))
;;   (lambda (x) 
;;     ((lambda (x) (inc (inc x)))
;;      (inc (inc x))))) 5)

;; (((lambda (x) ((lambda (f) (lambda (x) (f (f x))))
;;                ((lambda (f) (lambda (x) (f (f x)))) x)))
;;   (lambda (x) (inc (inc (inc (inc x)))))) 5)

;; (((lambda (f) (lambda (x) (f (f x))))
;;    ((lambda (f) 
;;       (lambda (x) (f (f x)))) 
;;     (lambda (x) (inc (inc (inc (inc x))))))) 5)

;; (((lambda (f) (lambda (x) (f (f x))))
;;   (lambda (x) ((lambda (x) (inc (inc (inc (inc x)))))
;;                ((lambda (x) (inc (inc (inc (inc x))))) x)))) 5)

;; (((lambda (f) (lambda (x) (f (f x))))
;;   (lambda (x) ((lambda (x) (inc (inc (inc (inc x)))))
;;                (inc (inc (inc (inc x))))))) 5)

;; (((lambda (f) (lambda (x) (f (f x))))
;;   (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)

;; ((lambda (x)
;;    ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))
;;     ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x))))))))) x))) 5)

;; ((lambda (x)
;;    ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))
;;     (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)

;; ((lambda (x)
;;    (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))) 5)


; We've FINALLY reached the end. Now we can wrap this up to get the answer...

;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 6)))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 7))))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 8)))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 9))))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 10)))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 11))))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc (inc 12)))))))))
;; (inc (inc (inc (inc (inc (inc (inc (inc 13))))))))    
;; (inc (inc (inc (inc (inc (inc (inc 14)))))))
;; (inc (inc (inc (inc (inc (inc 15))))))
;; (inc (inc (inc (inc (inc 16)))))
;; (inc (inc (inc (inc 17))))
;; (inc (inc (inc 18)))
;; (inc (inc 19))
;; (inc 20)
;; 21


; So we got the answer of 21, which agrees with what we get when we run
; the expression normally:

(((double (double double)) inc) 5)
;> 21