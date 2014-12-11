;;; SICP Exercise 1.10
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we looked at the procedure for computing Ackermann's
;;; function, and determined the values of some expressions.

; The procedure to compute Ackermann's function is as follows:

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


; With the procedure defined, we can simply run it to get the values of
; the given expressions:

(A 1 10)
;> 1024

(A 2 4)
;> 65536

(A 3 3)
;> 65536


; Alternatively, since we've been using the substitution model so much
; lately, we can work them out the hard way (if we hate the idea of fun):

;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
;; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
;; (A 0 (A 0 (A 0 (A 0 64))))
;; (A 0 (A 0 (A 0 128)))
;; (A 0 (A 0 256))
;; (A 0 512)
;; 1024

; Note that we have a recursive process here, of course.
; The second one is even worse (dots added for 'brevity').

;; (A 2 4)
;; (A 1 (A 2 3))
;; (A 1 (A 1 (A 2 2)))
;; (A 1 (A 1 (A 1 (A 2 1))))
;; (A 1 (A 1 (A 1 2)))
;; (A 1 (A 1 (A 0 (A 1 1))))
;; (A 1 (A 1 (A 0 2)))
;; (A 1 (A 1 4))
;; (A 1 (A 0 (A 1 3)))
;; (A 1 (A 0 (A 0 (A 1 2))))
;; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
;; (A 1 (A 0 (A 0 (A 0 2))))
;; (A 1 (A 0 (A 0 4)))
;; (A 1 (A 0 8))
;; (A 1 16)
;; (A 0 (A 1 15))
;; (A 0 (A 0 (A 1 14)))
;; ...
;; ...
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0  ... ...  (A 1 1))...)
;; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0  ... ...  2))...)
;; ...
;; ...
;; 65536

; So you can see how these things blow up dramatically.
; (The third one would be even worse, so let's not even go there.)


; Now suppose we have the following procedure definitions:

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

; We want to give precise mathematical definitions for these functions,
; in the way that (k n) computes 5n^2 obviously.


; Now (f n) expanded out looks like this:

;; (A 0 n)
;; (* 2 n)

; Simple enough. So (f n) is 2n.


; Next, (g n) expands something like this:

;; (A 1 n)
;; (A 0 (A 1 (- n 1)))
;; (A 0 (A 0 (A 1 (- n 2))))
;; ...
;; (A 0 (A 0 (A 0  ... (A 1 (- n (- n 1)))...)   ; With n-1 '(A 0' on the LHS
;; (A 0 (A 0 (A 0  ... (A 1 1))...)
;; (A 0 (A 0 (A 0  ... 2))...)
;; ...
;; (* 2 (* 2 (* 2 ... (* 2 2))...)   ; doubling 2, n-1 times.

; So (g n) works out to be (2^(n-1) * 2), or simply 2^n.


; Finally, we expand out (h n) to get the following:

;; (A 2 n)
;; (A 1 (A 2 (- n 1)))
;; (A 1 (A 1 (A 2 (- n 2))))
;; ...
;; (A 1 (A 1  ... (A 1 (A 1 (A 2 (- n (- n 1))...)  ; With n-1 '(A 1' on the LHS
;; (A 1 (A 1  ... (A 1 (A 1 2))...)
;; (A 1 (A 1  ... (A 1 (A 0 (A 1 1))...)
;; (A 1 (A 1  ... (A 1 (A 0 2))...)
;; (A 1 (A 1  ... (A 1 4))...)
;; (A 1 (A 1  ... 16))...)
;; (A 1 (A 1  ... 65536))...)
;; ...
;; ...
;; some huge number if n is anything larger than 4.

; Here we start with 2, and at each point we are raising 2 to the power of
; the result, n-1 times, which gives us huge numbers!
; So (h n) computes 2^2^...^2 (where there are n lots of 2 in that expression.)
