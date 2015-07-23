;;; SICP Exercise 3.5
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we defined a procedure to perform Monte Carlo
;;; integration, and used it to compute an estimate for pi.


; As given in the book, we'll use the 'random-in-range' procedure
; to return a random number in a given range:

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))


; We also have the abstract 'monte-carlo' procedure from the book:

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


; We can write our 'estimate-integral' procedure as follows.
; The procedure takes in a predicate P, the upper and lower bounds
; for the x- and y-values, and the number of trials. We can then
; define an internal procedure 'experiment' that takes no arguments
; and performs the task of picking a point in the rectangle and
; determining whether it satisfies the predicate.
;
; Then we simply pass the experiment procedure, together with the
; number of trials, straight to the 'monte-carlo' procedure.
;
; Finally, we multiply the resulting fraction by the total area
; of the rectangle to get our estimate for the integral.

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (let ((rectangle-area (* (abs (- x1 x2))
                           (abs (- y1 y2)))))
    (* rectangle-area (monte-carlo trials experiment))))


; And with our 'estimate-integral' procedure, we can now compute
; approximations to pi by estimating the integral of the unit circle
; (we can use the unit rectangle as our integration rectangle here):

(define (in-unit-circle? x y)
  (<= (+ (* x x) (* y y)) 1))

(define (estimate-pi trials)
  (estimate-integral in-unit-circle? -1 1 -1 1 trials))

(estimate-pi 5)
;> 3 1/5
; (ie. 3.20)

(estimate-pi 500)
;> 2 114/125
; (ie. 2.912)

(estimate-pi 500000)
;> 3 203/125000
; (ie. 3.001624)
