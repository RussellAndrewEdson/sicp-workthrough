;;; SICP Exercise 2.40
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we defined the procedure unique-pairs that
;;; takes an integer n and generates a sequence of pairs (i,j) with
;;; 1 <= j < i <= n.

; We have the flatmap and enumerate-interval procedures as they
; are defined in the book:

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; Now observe the old prime-sum-pairs procedure that we were given:

;;  (define (prime-sum-pairs n)
;;    (map make-pair-sum
;;         (filter prime-sum?
;;                 (flatmap
;;                  (lambda (i) 
;;                    (map (lambda (j) (list i j))
;;                         (enumerate-interval 1 (- i 1))))
;;                  (enumerate-interval 1 n)))))

; Here, the call to our flatmap procedure (with the lambda) is
; exactly the code we want for our unique pairs implementation!

; That is, we'll define unique-pairs as follows:

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

; And this works as we expect.
(unique-pairs 5)
;> ((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4))


; So then we could simplify our prime-sum-pairs procedure as follows:

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; And this will work (when we bring in -all- of the ancillary procedures...)

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
  
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  (* x x))

(prime-sum-pairs 6)
;> ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))
