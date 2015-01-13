;;; SICP Exercise 1.45
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we determine how many average damps are required 
;;; for convergence when finding the nth root using the fixed-point search,
;;; and use our 'repeated' procedure from Exercise 1.43 to implement this.


; First, we'll bring in all of the procedures from previous exercises that
; we'll need. 
;
; Note that we've also defined a special version of the fixed-point 
; procedure that we can more easily use for experiments. It prints the
; result at each stage, and takes in an additional 'iterations' parameter
; to limit the number of steps taken (so we don't loop forever on the 
; cases where we don't have convergence):

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) identity)
        (else (lambda (x) 
                ((compose f (repeated f (- n 1))) x)))))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-print f first-guess iterations)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (or (close-enough? guess next)
              (= count 0))
          next
          (try next (- count 1)))))
  (try first-guess (- iterations 1)))


; Now we want to find the fixed points of the transformation 
; y -> x/y^(n-1) to find the nth-root of x. So we can define this
; as a higher order procedure here:

(define (nth-root-function x n)
  (lambda (y) (/ x (expt y (- n 1)))))


; Now we can show here that the square root function doesn't converge
; without average damping (using the square root of 10 as an example).

(fixed-point-print (nth-root-function 10 2) 1.0 5)
;> 10.0
;> 1.0
;> 10.0
;> 1.0
;> 10.0

; So we see that it bounces between the guess and the number here.
; Applying average damping corrects this behaviour for us:

(fixed-point-print (average-damp (nth-root-function 10 2)) 1.0 5)
;> 5.5
;> 3.659090909090909
;> 3.196005081874647
;> 3.16245562280389
;> 3.162277665175675


; As stated in the book, we can see that the cube root doesn't converge
; either:

(fixed-point-print (nth-root-function 10 3) 1.0 5)
;> 10.0
;> 0.1
;> 999.9999999999998
;> 1.0000000000000004e-05
;> 99999999999.99992

; But it does converge when the average damping is applied.

(fixed-point-print (average-damp (nth-root-function 10 3)) 1.0 5)
;> 5.5
;> 2.915289256198347
;> 2.045955264231898
;> 2.2174544829878187
;> 2.1255858658796134


; Now for the fourth-root, we don't get convergence with the function
; on its own, nor with the average damp applied:

(fixed-point-print (nth-root-function 10 4) 1.0 5)
;> 10.0
;> 0.01
;> 9999999.999999998
;> 1.0000000000000005e-20
;> 9.999999999999984e+60

(fixed-point-print (average-damp (nth-root-function 10 4)) 1.0 10)
;> 5.5
;> 2.7800525920360633
;> 1.622734111272058
;> 1.9814803342967022
;> 1.6334289278412961
;> 1.9639940773315299
;> 1.6420054692022292
;> 1.9503986333631433
;> 1.6491061381456815
;> 1.9394229422620053

; Note here that even with the average damping applied, we still appear
; to be bouncing between two different values at each point.

; But applying two average damps in succession gives us what we want:
(fixed-point-print ((repeated average-damp 2) (nth-root-function 10 4)) 
                   1.0 
                   10)
;> 3.25
;> 2.5103265817023215
;> 2.040778500162044
;> 1.8247227303393658
;> 1.7800226459895
;> 1.7782819691816614
;> 1.7782794100444472


; So we have that:
;  1 average damp  is  required to compute the 2nd-root
;  1 average damp  is  required to compute the 3rd-root
;  2 average damps are required to compute the 4th-root

; We use the same process as above to determine the number of average
; damps required for 5th, 7th, 9th, 13th and 17th roots, running 20 iterations
; for each. The results are summarised in the table below.

; Avg damps  | 5th-root   | 7th-root   | 9th-root   | 13th-root   | 17th-root
; -----------------------------------------------------------------------------
;  0         | oscillates | oscillates | oscillates | oscillates  | oscillates
;  1         | oscillates | oscillates | oscillates | oscillates  | oscillates
;  2         | converges  | converges  | oscillates | oscillates  | oscillates
;  3         | converges  | converges  | converges  | converges   | oscillates
;  4         | converges  | converges  | converges  | converges   | converges
;  5         | converges  | converges  | converges  | converges   | converges

; At this point, we can look at the data and notice that for the nth-root, we
; seem to converge as long as the number of average damps is such that 
; 2^(# avg. damps) is the largest power of 2 still smaller than n.
;
; We can rewrite that to be more math-y, and posit the following conjecture:
;    To calculate the nth root, we need floor(log_2(n)) average damps.

; To provide some more evidence for this conjecture, we guess that the 50th
; root of 10 needs 5 or more successive average damps.

; So it should oscillate/diverge on the 4th average damp:

(fixed-point-print ((repeated average-damp 4) (nth-root-function 10 50)) 
                   1.0 
                   20)
;> 1.5625
;> 1.4648437501989293
;> 1.3732910205115865
;> 1.2874604427783383
;> 1.2069967888350512
;> 1.1316214736469197
;> 1.0623557009374136
;> 1.0282175758200598
;> 1.1238044337135198
;> 1.0556179779553323
;> 1.0337021996895766
;> 1.0922653851805508
;> 1.0322744016864207
;> 1.0995578198165967
;> 1.0368084835708826
;> 1.0783368859240785
;> 1.026461534848993
;> 1.1361232777017898
;> 1.066317927453341
;> 1.0265531759417437


; ... and it should converge on the 5th average damp.

(fixed-point-print ((repeated average-damp 5) (nth-root-function 10 50)) 
                   1.0 
                   20)
;> 1.28125
;> 1.2412126001084371
;> 1.202432583836961
;> 1.1648938797752848
;> 1.1286674759051618
;> 1.0942266887973089
;> 1.0638216942183676
;> 1.0456529402938206
;> 1.0480401225966256
;> 1.046645721616824
;> 1.047408727650481
;> 1.046973803866943
;> 1.0472164692744803
;> 1.0470793745606044
;> 1.0471562966089736
;> 1.0471129676235642
;> 1.0471373209180403
;> 1.0471236161263766
;> 1.0471313231477548
;> 1.0471313231477548


; So we seem to be right about our conjecture here! This means that we can 
; write a procedure to compute the nth root as follows (The book says to
; "assume the primitives", but I've included everything we need in separate
; procedures here.)

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n)))
                (nth-root-function x n))
               1.0))

; This procedure should compute nth roots successfully without the
; oscillations that we saw above.

(expt 12 34)
;>4922235242952026704037113243122008064

(nth-root 4922235242952026704037113243122008064 34)
;> 12.000000113426461
