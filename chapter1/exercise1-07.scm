;;; SICP Exercise 1.7
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we check out the limitations of that new-sqrt
;;; procedure and its good-enough? test. We then modify the procedure
;;; to monitor the change between iterations instead to see if it works
;;; better for small and large numbers.

(define (new-sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))


; Now the good-enough? test here isn't very effective for finding the
; square roots of very small numbers. Consider the following walkthrough
; of the code that would be executed for finding the square root of 0.00002:

(define a-small-number 0.00002)

; We start with a guess of 1.0:

(- (square 1.0) a-small-number)
;> 0.99998

; The test for the absolute difference will return false, so we improve
; our guess:

(improve 1.0 a-small-number)
;> 0.50001

; We test again:

(- (square 0.50001) a-small-number)
;> 0.24999000009999997

; ... and so on. The results of the continued iteration are tabulated below:

;   guess               |  (- (square guess) a-small-number) 
; ------------------------------------------------------------
; 1.0                   | 0.99998
; 0.50001               | 0.24999000009999997
; 0.25002499960000796   | 0.06249250042498398
; 0.12505249580046793   | 0.01561812670592605
; 0.06260621431702548   | 0.003899538071109326
; 0.03146283571116667   | 0.0009699100309878647

; The execution stops there, and returns 0.03146283571116667 as the best guess.
; Of course, if we can check that this answer is slightly off by squaring it:

(square 0.03146283571116667)
;> 0.0009899100309878647
; Notice that it's not equal to 0.00002.

; This problem occurs because our 'epsilon' value for the good-enough? procedure
; (ie. the value that we compare the difference against) is not small enough to
; deal with numbers that are this close to zero. The obvious solution would be
; to increase the precision by making this epsilon smaller, but then we would
; increase the number of iterations taken by the procedure for all numbers.


; On the other side of the number line, the test is inadequate for very large 
; numbers due to the limited precision associated with numerical operations 
; in computers. Observe the following search for the square root of a 
; large number like 1234567891011121314151617 :

(define a-big-number 1234567891011121314151617)

; As before, we'll run through the execution process for the new-sqrt procedure,
; and tabulate the results:

;   guess                 |  (- (square guess) a-big-number) 
; ------------------------------------------------------------
; 1.0                     | -1.2345678910111213e+24
; 6.172839455055607e+23   | 3.81039469378912e+47
; 3.0864197275278033e+23  | 9.5259867344728e+46
; 1.5432098637639017e+23  | 2.3814966836182e+46
; 7.716049318819508e+22   | 5.9537417090455e+45
; 3.858024659409754e+22   | 1.488435427261375e+45
; 1.929012329704877e+22   | 3.7210885681534375e+44
; 9.645061648524385e+21   | 9.302721420383594e+43
; 4.822530824262193e+21   | 2.3256803550958984e+43
; 2.4112654121310964e+21  | 5.814200887739746e+42
; 1.2056327060655482e+21  | 1.4535502219349365e+42
; 6.028163530327741e+20   | 3.633875554837341e+41
; 3.0140817651638705e+20  | 9.084688887093353e+40
; 1.5070408825819352e+20  | 2.2711722217733383e+40
; 7.535204412909676e+19   | 5.677930554433345e+39
; 3.767602206454839e+19   | 1.4194826386083358e+39
; 1.883801103227421e+19   | 3.5487065965208366e+38
; 9.419005516137138e+18   | 8.871766491302061e+37
; 4.709502758068635e+18   | 2.2179416228254846e+37
; 2.3547513790344484e+18  | 5.544854057063401e+36
; 1.1773756895174863e+18  | 1.3862135142655418e+36
; 5.886878447592675e+17   | 3.465533785660768e+35
; 2.943439223806823e+17   | 8.663834464121057e+34
; 1.471719611924383e+17   | 2.1659586159994e+34
; 7.358598060041346e+16   | 5.414896539689857e+33
; 36792990308595336.0     | 1.3537241346138223e+33
; 18396495171074884.0     | 3.384310333448137e+32
; 9198247619091874.0      | 8.460775802756143e+31
; 4599123876654801.0      | 2.1151939198248393e+31
; 2299562072545126.0      | 5.287984490920144e+30
; 1149781304707998.0      | 1.321995814088135e+30
; 574891189224743.9       | 3.304986448803493e+29
; 287446668352858.94      | 8.262435257926745e+28
; 143725481649381.72      | 2.0655779507455752e+28
; 71867035706422.48       | 5.163636253337193e+27
; 35942107103337.37       | 1.2906004951367635e+27
; 17988227946695.965      | 3.223417767712826e+26
; 9028429968638.611       | 8.027797980760068e+25
; 4582586112815.598       | 1.9765527590359246e+25
; 2425995150445.764       | 4.650884578975245e+24
; 1467443238641.5159      | 9.188217676235795e+23
; 1154374309149.5044      | 9.801215461327433e+22
; 1111921807462.4712      | 1.8022148994874202e+21
; 1111111402051.1409      | 6.567569305968312e+17
; 1111111106510.5996      | 87778394112.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
; 1111111106510.56        | -268435456.0
;  ...                    |   ...
;  ...                    |   ...


; Notice that after a while, the guess is not changing. This is because
; our improve procedure is dealing with such large numbers that it no
; longer has the precision required to differentiate them:

(/ a-big-number 1111111106510.56)
;> 1111111106510.5603

(average 1111111106510.56 1111111106510.5603)
;> 1111111106510.56

; So in this case, we end up in an infinite loop. (And note that 'fixing' the
; epsilon value so that the procedure works for small numbers as suggested 
; earlier would only make things worse here!)


; So what we can try to do, as suggested in the exercise, is to monitor the
; difference between successive guesses instead, and use that as our check
; to see whether the answer is good enough.
;
; Our new procedure to do this is iterates-unchanged?, and we pass it both
; the old guess and the new guess. (Note that we're computing the improved
; guess twice in the new iter procedure -- this is inefficient, but we
; haven't seen any of the ways to get around this in the book yet.)

(define (iterates-unchanged? old-guess new-guess)
  (< (abs (- old-guess new-guess)) 
     (* 0.001 old-guess)))

(define (newer-sqrt x)
  (new-sqrt-iter 1.0 x))

(define (new-sqrt-iter guess x)
  (if (iterates-unchanged? guess (improve guess x))
      guess
      (new-sqrt-iter (improve guess x)
                     x)))


; As we can see, this appears to work better for both the small numbers and
; the large numbers.

(newer-sqrt a-small-number)
;> 0.0044730776049973706

(square 0.0044730776049973706)
;> 2.0008423260329013e-05
; Approximately equal to 0.00002.

(newer-sqrt a-big-number)
;> 1111921807462.4712

(square 1111921807462.4712)
;> 1.2363701059106088e+24
; Still not too close, but at least it actually finishes this time!

; Note that here we could actually improve this by making our epsilon
; smaller (eg. 0.000001 instead of 0.001, say.)

