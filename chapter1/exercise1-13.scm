;;; SICP Exercise 1.13
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we are proving that Fib(n) is the closest 
;;; integer to phi^n / sqrt(5), using mathematical induction.

; We have that Fib(n) = 0                    if n = 0,
;                       1                    if n = 1,
;                       Fib(n-1) + Fib(n-2)  if n > 1.

; We have that phi = (1 + sqrt(5))/2.
; We also let  psi = (1 - sqrt(5))/2.

; So basically we want to show that Fib(n) = (phi^n - psi^n)/sqrt(5),
; as given.


; Now we have two base cases here:

; When n = 0, then Fib(n) = 0. 
; And (phi^0 - psi^0)/sqrt(5) = 0/sqrt(5) = 0, so the statement holds here.


; When n = 1, we have Fib(n) = 1.
; Then (phi^1 - psi^1)/sqrt(5) = ( (1 + sqrt(5))/2 - (1 - sqrt(5))/2 ) / sqrt(5)
;                              = ( 2 * (sqrt(5)/2) ) / sqrt(5)
;                              = sqrt(5) / sqrt(5)
;                              = 1
;
; So this statement holds too, and our base cases are satisfied.


; Now suppose that it is -true- that Fib(k) = (phi^k - psi^k)/sqrt(5), 
; for all 1 < k < m. (This is the strong form of mathematical induction.)


; We now show that Fib(m) = (phi^(m) - psi^(m))/sqrt(5) must then hold:
;
; So we have:
;  Fib(m) = Fib(m-1) + Fib(m-2)
;         = (phi^(m-1) - psi^(m-1))/sqrt(5) + (phi^(m-2) - psi^(m-2))/sqrt(5)
;         = ( (phi^(m-1) + phi^(m-2)) - (psi^(m-1) + psi^(m-2)) )/sqrt(5)
;         = ( phi^(m-2) * (phi + 1) - psi^(m-2) * (psi + 1) )/sqrt(5)  ............(*)


; Now note the following:
;  phi + 1 = (1 + sqrt(5))/2 + 1
;          = (1 + sqrt(5) + 2)/2
;          = (3 + sqrt(5))/2
;          = (2 * (3 + sqrt(5)))/4
;          = (6 + 2*sqrt(5))/4
;          = (1 + 2*sqrt(5) + 5)/4
;          = (1 + sqrt(5))^2 / 2^2
;          = ((1 + sqrt(5))/2)^2
;          = phi^2

; Similarly,
;  psi + 1 = (1 - sqrt(5))/2 + 1
;          = (1 - sqrt(5) + 2)/2
;          = (3 - sqrt(5))/2
;          = (2 * (3 - sqrt(5)))/4
;          = (6 - 2*sqrt(5))/4
;          = (1 - 2*sqrt(5) + 5)/4
;          = (1 - sqrt(5))^2 / 2^2
;          = ((1 - sqrt(5))/2)^2
;          = psi^2


; So substituting these values into our equation (*) above gives us:
;  Fib(m) = ( phi^(m-2) * (phi^2) - psi^(m-2) * (psi^2) )/sqrt(5)
;         = (phi^m - psi^m)/sqrt(5)

; As required.

; So by the principle of mathematical induction, we have that
;  Fib(n) = (phi^n - psi^n)/sqrt(5) is true for all n >= 0.