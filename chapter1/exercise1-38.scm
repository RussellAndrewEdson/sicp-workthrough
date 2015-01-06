;;; SICP Exercise 1.38
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used our cont-frac procedure from Exercise 1.37
;;; to compute an approximation to e based on Euler's continued fraction
;;; expansion.


; So we're given that Euler's continued fraction expansion is
; e - 2 = 1/(1 + (1/(2 + (1/(1 + (1/(1 + (1/(4 + (1/(1 + ...
;
; where each of the n values are 1, and the sequence for the d values
; is 1,2,1,1,4,1,1,6,1,1,8,1,1,10,1,1,12, .. etc.

; So the function to compute the n values is trivial:

(define (n k) 1)


; The function for the d values will require a bit more work.
; It helps to examine the sequence we have by lining the values up with
; the indices to see if we can notice a pattern:

;; d value: 1, 2, 1, 1, 4, 1, 1, 6, 1, 1,   8,  1,  1, 10, ...
;;   index: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, ...

; So we can start to see that the only non-1 values in the sequence
; will occur on the indices 2,5,8,11,14,17,20,23, ... etc.
; These are simply the multiples of 3 shifted back by 1.

; All good so far, but we also need a mapping that takes the index to the
; value when we have these shifted multiples of 3.
; That is, we need to find a function that transforms the indices as follows:
;    2 -> 2
;    5 -> 4
;    8 -> 6
;   11 -> 8
;   14 -> 10, ... etc.

; No obvious pattern yet. But remember that the numbers on the left-hand side
; were the multiples of 3 shifted back by 1. I wonder what happens when we
; move then forward again?
;    3 -> 2
;    6 -> 4
;    9 -> 6
;    12 -> 8
;      ...

; Now we're getting somewhere! We've got the multiples of 3 clearly on the 
; left-hand side and the multiples of 2 on the right-hand side. And they 
; match up exactly (ie. the first multiple of 3 maps to the first multiple
; of 2, the second multiple of 3 maps to the second multiple of 2, and so on.)

; We're pretty much there at this point. All we need to do now is divide the 
; entire left-side column by 3:
;    1 -> 2
;    2 -> 4
;    3 -> 6
;    4 -> 8
;     ...

; And we've arrived. Now a pattern is obvious.

; So given an index n that when shifted forward by 1 gives us a multiple of 3, 
; to arrive at the value we need to give it, we:
;   1. Shift the index forward by 1. (ie. add 1.)
;   2. Return the multiple of 3 that it corresponds to
;      (that is, return the number of times that 3 goes into that number,
;       ie. divide by 3!)
;   3. Finally, multiply by 2.

; This translates into code easily enough.


; So let's bring in that 'divides?' method that we had before when we 
; were looking at primes, and use it to define a procedure to generate
; the elements of this sequence:

(define (divides? a b)
  (= (remainder b a) 0))

(define (d k)
  (if (divides? 3 (+ k 1))
      (* 2 (/ (+ k 1) 3))
      1))

; This will give us what we want:

(d 1)
;> 1

(d 2)
;> 2

(d 3)
;> 1

(d 4)
;> 1

(d 5)
;> 4

; ...

(d 8)
;> 6
     

; So then the last thing to do is to bring in our cont-frac procedure
; from Exercise 1.37 and use it to define a procedure that approximates
; e with a k-term:

(define (cont-frac n d k)
  (define (frac-iter n d count frac)
    (if (= count 0)
        frac
        (frac-iter n
                   d
                   (- count 1)
                   (/ (n count) (+ (d count) frac)))))
  (frac-iter n d k 0))

(define (e-approximation k)
  (+ 2.0 (cont-frac n d k)))


; We can now approximate e using whatever values of k that we want!

(e-approximation 1)
;> 3.0

(e-approximation 5)
;> 2.71875

(e-approximation 10)
;> 2.7182817182817183

(e-approximation 50)
;> 2.718281828459045