;;; SICP Exercise 1.31
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we made a product high-order procedure that works
;;; the same way as the sum procedure, and used it to define the factorial
;;; function, as well as to compute the Wallis product for pi.


; The higher-order product procedure is written exactly like the
; summation procedure we saw earlier. In fact, the only real difference
; is that we're multiplying (and so we have 1 as the identity
; element now):

(define (recursive-product term a next b)
  (if (> a b)
      1
      (* (term a)
         (recursive-product term (next a) next b))))


; The iterative process procedure is written exactly the same way
; as sum was in Exercise 1.30; again, keeping in mind that we use
; 1 instead of 0 for the identity now.

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (* result (term a)))))
  (iter a 1))


; We can very easily define the factorial procedure in terms of
; our new product procedure:

(define (inc x)
  (+ x 1))

(define (factorial n)
  (product identity 1 inc n))


; The factorial procedure works exactly as we'd want it to.

(factorial 4)
;> 24

(factorial 9)
;> 362880


; Now we're given that an approximation for pi can be found by
; truncating the expression:
;
; pi/4 = (2*4*4*6*6*8*8*10*10* ...) / (3*3*5*5*7*7*9*9* ...)
;
; A novel way of doing this is to look at the product as the following:
;
; pi/4 = (2/3)*(4/3)*(4/5)*(6/5)*(6/7)*(8/7)*(8/9)* ...
;
; Then note that the 1st, 3rd, 5th, etc. elements in this sequence are
;    (2/3), (4/5), (6/7), (8/9), (10/11), ...
;      1      3      5      7       9
;
; And so we see that for an odd index n, the element is given by
;   f(n) = (n+1)/(n+2).
;
; Similarly, the even index elements are
;    (4/3), (6/5), (8/7), (10/9), ...
;      2      4      6       8
;
; So for an even index n, the element is given by
;   f(n) = (n+2)/(n+1).
;
; This all translates very neatly into an expression to generate the
; nth term:

(define (nth-term n)
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))

; Then we can approximate pi using n of these ratios as follows:

(define (pi-approx n)
  (* 4.0 (product nth-term 1 inc n)))


(pi-approx 1000)
;> 3.1431607055322663

(pi-approx 10000)
;> 3.1417497057380523

;(pi-approx 100000)
;> 3.1416083612781764
; (This last one takes a while!)