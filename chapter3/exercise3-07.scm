;;; SICP Exercise 3.7
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we modified the password-protected bank account
;;; object of Exercise 3.3 to support joint account aliasing.


; Recall our password-protected bank account object from Exercise 3.3.
; To help us with this exercise, we'll define a new behaviour for the
; object: 'correct-password?', which takes a symbol and returns true if
; the symbol matches the given password, and false otherwise.

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password amount)
    "Incorrect password")
  (define (correct-password? try-password)
    (eq? try-password password))
  (define (dispatch p m)
    (if (eq? m 'correct-password?)
        (correct-password? p)
        (cond ((not (eq? p password)) incorrect-password)
              ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))))
  dispatch)


; We can define a procedure 'make-joint' that makes a joint account
; as follows. The arguments to the procedure will be a given
; password-protected account, the password that accesses that account,
; and a new password. The procedure returns an alias to that account
; that is accessed using the new password.
;
; Here we can simply set up a 'facade' of sorts for the account; we
; return a new procedure that checks for the new password, and if
; the symbols match, simply passes the correct -old- password to the
; existing account.

(define (make-joint account old-password new-password)
  (define (incorrect-password amount)
    "Incorrect password")
  (if (account old-password 'correct-password?)
      (lambda (p m)
        (if (not (eq? p new-password)) 
            incorrect-password
            (account old-password m)))
      "Incorrect password -- no joint access authorized."))


; And we can test our new joint bank-account functionality as follows.
; Here we have an account peter-acc, an account paul-acc that is joint
; with peter-acc, and an account penny-acc that tries to be joint with
; peter-acc, but fails due to an incorrect password.

(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'wrong-password 'withdraw) 10)
;> "Incorrect password"

((peter-acc 'open-sesame 'withdraw) 10)
;> 90

(define penny-acc (make-joint peter-acc 'wrong-password 'rosebud))
penny-acc
;> "Incorrect password -- no joint access authorized."

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'open-sesame 'withdraw) 20)
;> "Incorrect password"

((paul-acc 'rosebud 'withdraw) 20)
;> 70

((peter-acc 'rosebud 'deposit) 10)
;> "Incorrect password"

((peter-acc 'open-sesame 'deposit) 10)
;> 80

; Notice here that peter-acc and paul-acc refer to the same account object.