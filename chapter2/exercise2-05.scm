;;; SICP Exercise 2.5
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us representing pairs of nonnegative integers using
;;; only numbers and arithmetic operations, and defining the corresponding
;;; cons, car and cdr procedures.


; Suppose we represent a pair of nonnegative numbers (a,b) as the integer 
; that is the product 2^a * 3^b. So mathematically, suppose we have a function
; f that maps a pair (a,b) -> 2^a * 3^b.


; Now in order for our pair representation to make sense at all, our 
; function f needs to be one-to-one (otherwise we could have multiple
; pairs a,b for the same integer representation, which wouldn't work!)

; So we'll quickly show that f is indeed one-to-one:
;
; Let f(a,b) = f(a',b') for nonnegative integers a,a',b,b'.
; Then we have that:
;      2^a * 3^b = 2^a' * 3^b'
;     2^a / 2^a' = 3^b' / 3^b
;       2^(a-a') = 3^(b'-b)      (using index laws.)
;
; Now let's consider 2^(a-a'). Depending on whether a > a' or the
; other way around, we've always got an even number or the reciprocal of
; an even number here (since 2 is always a factor.)
;
; But now look at 3^(b'-b). A positive integer power of 3 is always going
; to be odd (Why? Well, we can write 3^x as (2+1)^x, and when we expand
; it out we get the following:
;    (2+1)^x = 2^x + (BC)*2^(x-1)*1^1 + ... + (BC)*2^1*1^(x-1) + 1^x
;            = 2*(some number) + 1.
;
; Here (BC) stands for the binomial coefficients, which we don't care
; about. Notice the end result though. We have 2*(some number), which
; is even. Then adding 1 makes it odd, as we suggested.)
;
; So 3^(b'-b) is either an odd number or the reciprocal of an odd number
; depending on whether b' > b or not. 
;
; So 2^(a-a') is never equal to 3^(b'-b) except when the exponent is 0,
; ie. a-a' = 0 = b'-b. This occurs exactly when a=a' and b=b'. So we're
; done: if ever f(a,b) = f(a',b'), we must have that a=a' and b=b'.
; (That is, f is one-to-one as required.)


; So that's good news; we have a unique representation 2^a * 3^b for any
; pair (a,b). Then we can define cons, car and cdr as follows (I've named
; then differently so as to not clash with the existing cons, car, cdr
; definitions.)

(define (cons2 a b)
  (* (expt 2 a)
     (expt 3 b)))

; To get back a, we need to divide out as many powers of 2 as we can.
; We'll use that handy divides? procedure we've seen before:

(define (divides? a b)
  (= (remainder b a) 0))

(define (car2 z)
  (define (div2-iter num powers)
    (if (divides? 2 num) 
        (div2-iter (/ num 2) (+ powers 1))
        powers))
  (div2-iter z 0))

; Then to get back b, we do the same thing, except with powers of 3.

(define (cdr2 z)
  (define (div3-iter num powers)
    (if (divides? 3 num)
        (div3-iter (/ num 3) (+ powers 1))
        powers))
  (div3-iter z 0))


; So we can try this out with a few examples:

; The pair (2,4) is represented by 2^2 * 3^4 = 324:
(cons2 2 4)
;> 324

(car2 324)
;> 2

(cdr2 324)
;> 4


(car2 (cons2 23 12))
;> 23

(car2 (cons2 123 22))
;> 123

(cdr2 (cons2 213 111))
;> 111

(cdr2 (cons2 11 410))
;> 410
