;;; SICP Exercise 3.4
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we continued modifying the 'make-account'
;;; procedure of Exercise 3.3 so that it kept track of incorrect
;;; password tries.


; The only changes we need to make to the procedure from Exercise 3.3
; are to construct the local variable to keep track of the number
; of consecutive incorrect password attempts, update that count in our
; internal 'incorrect-password' procedure (calling our 'call-the-cops'
; procedure if we have more than 7), and reset the count if we ever
; have a correct password.

(define (make-account balance password)
  (let ((consecutive-incorrect-passwords 0))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password amount)
      (set! consecutive-incorrect-passwords
            (+ consecutive-incorrect-passwords 1))
      (if (> consecutive-incorrect-passwords 7)
          (call-the-cops)
          "Incorrect password"))
    (define (call-the-cops)
      "More than 7 consecutive incorrect password attempts. Cops notified.")
    (define (dispatch p m)
      (if (not (eq? p password)) 
          incorrect-password
          (begin (set! consecutive-incorrect-passwords 0)
                 (cond ((not (eq? p password)) incorrect-password)
                       ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT"
                                    m))))))
    dispatch))


; And that's it. Now when an account is accessed with an incorrect
; password more than seven times consecutively, the 'call-the-cops'
; procedure is called.

(define acc (make-account 100 'secret-password))

; Accessed with incorrect password 5 times:
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
;> ... 
;> "Incorrect password"
;> ...

; Correct password access (the counter is reset here.)
((acc 'secret-password 'withdraw) 40)
;> 60

; Incorrect access 8 times (call-the-cops procedure called.)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
((acc 'some-other-password 'withdraw) 40)
;> ... 
;> "Incorrect password"
;> ...
;> "More than 7 consecutive incorrect password attempts. Cops notified."
