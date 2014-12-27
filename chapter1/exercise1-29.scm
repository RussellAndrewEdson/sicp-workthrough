;;; SICP Exercise 1.29
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to write a procedure to implement
;;; Simpson's Rule for numerical integration, then compare the 
;;; results of integrating the cube function between 0 and 1 with
;;; those obtained from the other integration method outlined in 
;;; the book.


; We were shown the higher-order function 'sum' in the book, which
; gives us an elegant way to define stuff like this:

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


; Then the integration procedure defined in the book is as follows:

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


; Now Simpson's Rule approximates the integral of a function f between
; a and b as follows:
;
; int f = (h/3) * ( y_0 + 4y_1 + 2y_2 + 4y_3 + 2y_4 + 4y_5 + ... 
;                          + 2y_(n-2) + 4y_(n-1) + y_n )
;
;       = (h/3) * ( y_0 + 4(y_1 + y_3 + ... + y_(n-1))
;                       + 2(y_2 + y_4 + ... + y_(n-2))
;                       + y_n )
;
; where h = (b-a)/n and y_k = f(a+kh), for some even integer n.
;
; So basically what we want to do is sum up the even indices (not equal to
; k=0 or k=n) and multply them by 2; sum up the odd indices and multiply 
; them by 4, then add on the k=0 and k=n indices and return the result of
; multiplying that whole thing by h/3.


; So we can define that as follows (using block structure for the ancillary
; h and y_k values):

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (plus-two n) (+ n 2))
  (* (/ h 3.0) (+ (* 2 (sum y 2 plus-two (- n 2)))
                  (* 4 (sum y 1 plus-two (- n 1)))
                  (y 0)
                  (y n))))


; Now we'll test this procedure by integrating the cube function 
; between 0 and 1:

(define (cube x) 
  (* x x x))

; The original integral function gives us the following 
; (with dx=0.01, dx=0.001):

(integral cube 0 1 0.01)
;> 0.24998750000000042

(integral cube 0 1 0.001)
;> 0.249999875000001

; Our Simpson's rule procedure gives us the following 
; (with n=100, n=1000):

(simpson cube 0 1 100)
;> 0.25

(simpson cube 0 1 1000)
;> 0.25

; As we can see, the Simpson's rule approximation matches 0.25 exactly,
; and so seems to be more accurate than the other integral method.
