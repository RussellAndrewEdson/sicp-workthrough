;;; SICP Exercise 1.32
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; With this exercise we took the commonalities between our 'sum'
;;; and 'product' procedures and used them to write a more general
;;; accumulation procedure.


; The recursive sum and product procedures are shown below.

;;  (define (recursive-sum term a next b)
;;    (if (> a b)
;;        0
;;        (+ (term a)
;;           (recursive-sum term (next a) next b))))

;;  (define (recursive-product term a next b)
;;    (if (> a b)
;;        1
;;        (* (term a)
;;           (recursive-product term (next a) next b))))

; We can notice that they share a very similar structure, and use
; that to create the more general recursive accumulate procedure:

(define (recursive-accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (recursive-accumulate combiner 
                                      null-value 
                                      term 
                                      (next a) 
                                      next 
                                      b))))

; Then creating the recursive sum and product procedures is easy:

(define (recursive-sum term a next b)
  (recursive-accumulate + 0 term a next b))

(define (recursive-product term a next b)
  (recursive-accumulate * 1 term a next b))


; These still work as expected.

(define (inc x) (+ x 1))

(recursive-sum identity 1 inc 10)
;> 55

(recursive-product identity 1 inc 10)
;> 3628800


; We can also define the accumulate procedure to generate an iterative
; process by comparing the similarities between the iterative versions
; of the sum and product procedures we had before:

;;  (define (sum term a next b)
;;    (define (iter a result)
;;      (if (> a b)
;;          result
;;          (iter (next a) 
;;                (+ result (term a)))))
;;    (iter a 0))

;;  (define (product term a next b)
;;    (define (iter a result)
;;      (if (> a b)
;;          result
;;          (iter (next a) 
;;                (* result (term a)))))
;;    (iter a 1))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

; The definition of sum and product is exactly the same:

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


; These ones work, too.

(sum identity 1 inc 100)
;> 5050

(product identity 1 inc 4)
;> 24