;;; SICP Exercise 1.20
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise we wanted to use the substitution model to illustrate
;;; the process generated in evaluating (gcd 206 40) in both normal-order
;;; and applicative-order evaluation, and count the number of remainder
;;; operations used in each.

; The gcd procedure is here:
; (Commented out to avoid a name clash, since gcd is already defined)

;;  (define (gcd a b)
;;    (if (= b 0)
;;        a
;;        (gcd b (remainder a b))))


; The evaluation of the (gcd 206 40) expression under normal-order
; evaluation starts off nicely enough:

;;  (gcd 206 40)
;;  (gcd 40 (remainder 206 40))

; The key thing to note here is that a=40, but b=(remainder 206 40), which
; doesn't get worked out until exactly when it's needed. So when we go into
; the body of the procedure for the next gcd application, it looks like this:

;;  (if (= (remainder 206 40) 0)
;;      40
;;      (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

; At this point we perform one remainder operation (in the if
; equality check).
; Note also the duplication of the (remainder 206 40) term.


; So now a=(remainder 206 40)
;        b=(remainder 40 (remainder 206 40))
;
; We keep going:

;;  (if (= (remainder 40 (remainder 206 40)) 0)
;;      (remainder 206 40)
;;      (gcd (remainder 40 (remainder 206 40))
;;           (remainder (remainder 206 40) 
;;                      (remainder 40 (remainder 206 40)))))

; In this if check, we perform 2 remainder operations.


; We now have a=(remainder 40 (remainder 206 40))
;             b=(remainder (remainder 206 40) 
;                          (remainder 40 (remainder 206 40)))
;
; It just keeps getting worse from here:

;;  (if (= (remainder (remainder 206 40) 
;;                    (remainder 40 (remainder 206 40)))
;;         0)
;;      (remainder 40 (remainder 206 40))
;;      (gcd (remainder (remainder 206 40) 
;;                      (remainder 40 (remainder 206 40)))
;;           (remainder (remainder 40 (remainder 206 40))
;;                      (remainder (remainder 206 40) 
;;                                 (remainder 40 (remainder 206 40))))))

; So we've performed an additional 4 remainder operations in the if check,
; and the remainder expressions are building up in a and b each time.


; For the next run, we have:
;  a=(remainder (remainder 206 40) 
;               (remainder 40 (remainder 206 40)))
;
;  b=(remainder (remainder 40 (remainder 206 40))
;               (remainder (remainder 206 40) 
;                          (remainder 40 (remainder 206 40))))
;
; Thankfully, we are at the end of the process with the next if expression:

;;  (if (= (remainder (remainder 40 (remainder 206 40))
;;                    (remainder (remainder 206 40) 
;;                               (remainder 40 (remainder 206 40))))
;;         0)
;;      (remainder (remainder 206 40)
;;                 (remainder 40 (remainder 206 40)))
;;      (gcd (remainder (remainder 40 (remainder 206 40))
;;                      (remainder (remainder 206 40)
;;                                 (remainder 40 (remainder 206 40))))
;;           (remainder (remainder (remainder 206 40)
;;                                 (remainder 40 (remainder 206 40)))
;;                      (remainder (remainder 40 (remainder 206 40))
;;                                 (remainder (remainder 206 40)
;;                                            (remainder 40 (remainder 206 40)))))))

; At this point we perform an additional 7 remainder operations in the
; if check. But since the result is 0, we return a:

;;  (remainder (remainder 206 40)
;;             (remainder 40 (remainder 206 40)))

; At this point, we start wrapping things up.


; So we had 1+2+4+7 = 14 remainder operations in the if checks, and then
; another 4 remainder operations to get the final answer.
; All up, that's 18 remainder operations!
; (Also notice how much worse the next iteration would have been if we'd
; had one...)


; Compare this with applicative-order evaluation:

;;  (gcd 206 40)
;;  (gcd 40 (remainder 206 40))
;;  (gcd 40 6)
;;  (gcd 6 (remainder 40 6))
;;  (gcd 6 4)
;;  (gcd 4 (remainder 6 4))
;;  (gcd 4 2)
;;  (gcd 2 (remainder 4 2))
;;  (gcd 2 0)
;;  2
                    
; We only use 4 remainder operations here.
