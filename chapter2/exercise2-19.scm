;;; SICP Exercise 2.19
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we modified the change-counting program from
;;; back in Chapter 1 (which we explored in Exercise 1.14) so that
;;; we could pass in a list of coins to use (eg. so we could use
;;; both US and UK currencies with the procedure).


; The original change-counting code was the following:

;;  (define (count-change amount)
;;    (cc amount 5))
;;
;;  (define (cc amount kinds-of-coins)
;;    (cond ((= amount 0) 1)
;;          ((or (< amount 0) (= kinds-of-coins 0)) 0)
;;          (else (+ (cc amount
;;                       (- kinds-of-coins 1))
;;                   (cc (- amount
;;                          (first-denomination kinds-of-coins))
;;                       kinds-of-coins)))))
;;
;;  (define (first-denomination kinds-of-coins)
;;    (cond ((= kinds-of-coins 1) 1)
;;          ((= kinds-of-coins 2) 5)
;;          ((= kinds-of-coins 3) 10)
;;          ((= kinds-of-coins 4) 25)
;;          ((= kinds-of-coins 5) 50)))


; You can see that we have the knowledge of the currency used distributed
; between the first-denomination and count-change procedures. We want to
; rewrite the cc procedure so that it instead takes a list of coins to
; use as its second argument, as is done in the book:

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))


; So we need to define the procedures: first-denomination, no-more? and
; except-first-denomination.


; For the first-denomination procedure, if we look back up at the way it
; is defined in the previous version of the change-counting program, we
; can see that it just returns a different denomination based on the 
; given integer.

; If we assume for a second that we'll be cdring down the list to
; get to the other coins, then we're really just returning the car of
; the list at each point. So our first-denomination procedure is easy:

(define (first-denomination coins)
  (car coins))


; Next, we'll tackle the except-first-denomination procedure. If our
; 'first-denomination' was the first coin in the list as we just defined
; it to be, then this procedure is also simple. We'll just be returning
; the cdr of the coins list at each point! (So we -are- cdring down the
; coins list after all.)

(define (except-first-denomination coins)
  (cdr coins))


; Then the last procedure to look at will be the no-more? procedure.
; If we compare the new cc procedure to the old one, we see that we
; need no-more? to basically return true when we're out of coins.
; So this is just as easy as a check for the empty list:

(define (no-more? coins)
  (null? coins))


; So our new cc procedure is complete. We can test it with the given
; example:

(define us-coins (list 50 25 10 5 1))
(cc 100 us-coins)
;> 292

; We can also use the UK coins though, in a small example (so we can
; verify that we get the correct answer):

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 2 uk-coins)
;> 4
; ie. 2 = 2, 
;     2 = 1 + 1, 
;     2 = 1 + 0.5 + 0.5, 
;     2 = 0.5 + 0.5 + 0.5 + 0.5 .


; Finally, if you check the cc procedure you'll see that nowhere do
; we actually require that the coins be in a specific order. As part
; of the combinatorial logic, all we care about at any point is:
;   - the number of ways to make change if we don't use a particular
;     type of coin,
;   - the number of ways to make change if we did use the coin.
;
; The actual value of the coin in relation to the others doesn't matter
; at all, and so the order of the coins can be arbitrary:

(define reverse-order (list 1 5 10 25 50))
(cc 100 reverse-order)
;> 292

(define different-order (list 1 50 25 10 5))
(cc 100 different-order)
;> 292
