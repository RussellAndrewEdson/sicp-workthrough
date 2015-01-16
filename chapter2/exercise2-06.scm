;;; SICP Exercise 2.6
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us exploring the Church numerals, where
;;; the nonnegative integers and their addition are defined completely
;;; in terms of procedure applications.

; We are given the implementations for 0 and incrementing as follows:

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))


; Let's use the substitution model to evaluate (add-1 zero):

;;  (add-1 zero)
;;  (add-1 (lambda (f) (lambda (x) x)))
;;  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;;  (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;;  (lambda (f) (lambda (x) (f x)))

; So we have the following definition for 1:

(define one (lambda (f) (lambda (x) (f x))))


; We can continue using the substitution model to get an expression
; for 2:

(add-1 one)
(add-1 (lambda (f) (lambda (x) (f x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

; So 2 is defined to be:

(define two (lambda (f) (lambda (x) (f (f x)))))


; So with this, we can notice a pattern.

; A number is going to look like 
;   (lambda (f) (lambda (x) (f (f (f ... (f x))...)
; Where the number of times f appears dictates which number it is.

; So given that, the addition of two numbers becomes a simple
; mathematical function composition. Given numbers a and b, we
; define a procedure that takes in a procedure f, and returns a
; procedure x that applies (b f) to x (which isolates the 
; (f (f .. (f x)...)) expression), and then apply (a f) to that,
; which will effectively compose them. Then we just return that
; expression.

; Sound complicated, but here it is:

(define (add-nums a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


; We can test this out. From the pattern above, we figure we can
; represent the numbers 3, 4 and 7 as the following (note the number
; of times f appears in each expression!)

(define three (lambda (f) (lambda (x) (f (f (f x))))))
(define four (lambda (f) (lambda (x) (f (f (f (f x)))))))
(define seven (lambda (f) (lambda (x) (f (f (f (f (f (f (f x))))))))))


; Then using the substitution model, adding three and four should give 
; us back seven as it appears above.

;;  (add-nums three four)
;;
;;  (add-nums (lambda (f) (lambda (x) (f (f (f x)))))
;;            (lambda (f) (lambda (x) (f (f (f (f x)))))))
;;
;;  (lambda (f) (lambda (x) (((lambda (f) (lambda (x) (f (f (f x))))) f)
;;                           (((lambda (x) (f (f (f (f x))))) f) x))))
;;
;;  (lambda (f) (lambda (x) ((lambda (x) (f (f (f x))))
;;                           ((lambda (x) (f (f (f (f x))))) x))))
;;
;;  (lambda (f) (lambda (x) ((lambda (x) (f (f (f x))))
;;                           (f (f (f (f x)))))))
;;
;;  (lambda (f) (lambda (x) (f (f (f (f (f (f (f x)))))))))


; So our results for 3+4=7 match.
