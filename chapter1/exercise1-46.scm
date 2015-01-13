;;; SICP Exercise 1.46
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wrote the abstract procedure iterative-improve
;;; that implemented the notion of starting with a guess, checking whether
;;; it is good enough, and then moving toward a better guess if not.


; We want the iterative-improve procedure to take in two methods:
; a method that dictates whether we've reached a good enough solution,
; and a method that improves the old solution. The procedure then returns
; another procedure that takes a guess and performs the iterative 
; improvement.

; For both procedures, we'll say that they take only one argument
; (the guess) -- even though the procedures we want to rewrite both
; have their 'good-enough?' procedure take 2 arguments. The reason
; for this is that we want to be completely general -- we don't want
; to force any assumptions about how the guess is determined to be 
; sufficient. 
;
; (Besides we don't need that second argument when we can use closures,
; as you'll see!)

; Then the procedure can be written in a similar way to how we usually
; write iterative processes:

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (improve-iter result)
      (if (good-enough? result)
          result
          (improve-iter (improve result))))
    (improve-iter guess)))
        

; We can then rewrite the sqrt procedure as follows. Note in particular that
; our 'good-enough?' and 'improve' procedures are closures containing x,
; so they are still comparative, even though there's only one argument!

(define (new-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))
  
(define (square x)
  (* x x))
  
(define (average x y)
  (/ (+ x y) 2))

; We can then use the procedure in the normal way.

(new-sqrt 2)
;> 1.4142156862745097

(sqrt 2)
;> 1.4142135623730951


; Similarly, we can rewrite the fixed-point procedure. Here
; we use the fact that we're comparing the guess to the value of
; the function at guess to determine the fixed point, so we can
; get both of these pieces of information from the one parameter
; since we have the function f accessible in the closure.
; (This does mean we end up doing the calculation twice though.)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

; This procedure works as normal too. (For the most part! There
; is a subtle difference; whereas before we returned the -next-
; guess if our guess was good-enough, here we instead return the
; -current- guess. So we get a slightly different answer!

(fixed-point cos 1.0)
;> 0.7390893414033927

;; Compared with the one from the book:
;> 0.7390822985224023
