;;; SICP Exercise 2.14
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we investigated the behaviour of our interval
;;; arithmetic system for particular arithmetic expressions.

; All of our interval arithmetic code is as follows (for simplicity
; we'll use the original mul-interval procedure though.)

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percentage)
  (let ((tolerance (* center percentage (/ 1 100))))
    (make-center-width center tolerance)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (interval-contains? x interval)
  (<= (lower-bound interval) x (upper-bound interval)))

(define (div-interval x y)
  (if (interval-contains? 0 y)
      (error "Interval contains zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

; We'll also define a pretty-printing procedure so that we can
; more easily see the results of our tests. (We'll print them
; in center-percent form.)

(define (print-interval i)
  (newline)
  (display (center i))
  (display " ")
  (display (percent i))
  (display "%"))


; First, we need to investigate the claims that the parallel-resistance
; formulae give different results:

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; Let's test (20-ohm 5%) and (100-ohm 1%):

(define interval-a (make-center-percent 20 1))
(define interval-b (make-center-percent 100 5))

(print-interval (par1 interval-a interval-b))
;> 16.749785708719706 10.303558967201669%

(print-interval (par2 interval-a interval-b))
;> 16.66295599514644 1.6680028058923633%

; We can notice that we get very different values for the percentage
; tolerance of the result interval. So we do actually get two different
; answers for the algebraically-equivalent expressions.


; Let's try (interval-a / interval-a) and (interval-a / interval-b):

(print-interval (div-interval interval-a interval-a))
;> 1.0002000200020003 1.9998000199979908%

(print-interval (div-interval interval-a interval-b))
;> 0.2006015037593985 5.997001499250372%

; So we start to get an idea of what is going on with our par1 and par2
; procedures above. Look at our (interval-a / interval-a) calculation.
; If the interval represents a number with a given tolerance, then surely
; dividing the interval by itself should give us 1.0 with a 0% error, right?

; But that's not what happens. Recall that the entire interval arithmetic 
; system was defined in terms of minimum and maximum values. So whereas in
; an expression like A/A we expect to represent the same value, the system 
; actually considers how "far apart" it can pick the values in the interval,
; and then returns us -this- as an answer instead.

; In regards to our parallel-resistance equations, this results in the
; different answers: the first (R1*R2)/(R1+R2) equation assumes that both
; references of R1 (or R2) are the same value, whereas our interval arithmetic
; system will not. Compare this with the second 1/((1/R1) + (1/R2)) equation,
; where the 'sameness' of the values has already been factored out by the 
; algebraic rearrangement.
