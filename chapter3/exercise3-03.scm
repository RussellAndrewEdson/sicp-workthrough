;;; SICP Exercise 3.3
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we modified the book's 'make-account'
;;; procedure to create password-protected accounts.

; The 'make-account' procedure from the book is as follows.

;;  (define (make-account balance)
;;    (define (withdraw amount)
;;      (if (>= balance amount)
;;          (begin (set! balance (- balance amount))
;;                 balance)
;;          "Insufficient funds"))
;;    (define (deposit amount)
;;      (set! balance (+ balance amount))
;;      balance)
;;    (define (dispatch m)
;;      (cond ((eq? m 'withdraw) withdraw)
;;            ((eq? m 'deposit) deposit)
;;            (else (error "Unknown request -- MAKE-ACCOUNT"
;;                         m))))
;;    dispatch)


; Now we want to modify this 'make-account' procedure to take
; an additional argument that specifies a symbol to be used
; for the password. Then whenever a request is made a password
; is supplied, and we only process the request for a correct
; password.
;
; So all we need to change internally is our dispatch procedure:
; we first check whether the passwords match. If they don't, we
; return a procedure that signals that the password was incorrect.

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
  (define (dispatch p m)
    (cond ((not (eq? p password)) incorrect-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)


; And our new 'make-account' procedure creates accounts that work as follows.

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)
;> 60

((acc 'some-other-password 'deposit) 50)
;> "Incorrect password"