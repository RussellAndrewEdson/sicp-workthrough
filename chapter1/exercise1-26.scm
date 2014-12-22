;;; SICP Exercise 1.26
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to investigate the apparent time difference
;;; between using the old expmod method that we'd seen in the book, and
;;; a new expmod procedure that doesn't call square (instead it just 
;;; multiplies the thing by itself inline.)


; Since it's always cool to see some actual numbers with this sort of stuff,
; we'll compare the two expmod procedures with the timed-prime-test cases
; that we actually had in Exercise 1.24.

; The "right" code that we had in Exercise 1.24 is as follows:

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (square x)
  (* x x))


; The new expmod procedure to use looks like this.
; (Note the lack of the call to the square procedure, and instead
; we multiply (slow-expmod base (/ exp 2) m) by itself using the
; '*' procedure.)

(define (slow-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (slow-expmod base (/ exp 2) m)
                       (slow-expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (slow-expmod base (- exp 1) m))
                    m))))


; Now we'll take the test code and modify it for use with the
; "slower" expmod procedure.

(define (slow-timed-prime-test n)
  (newline)
  (display n)
  (start-slow-prime-test n (runtime)))

(define (start-slow-prime-test n start-time)
  (if (slow-prime? n 1)
      (report-prime (- (runtime) start-time))))

(define (slow-prime? n times)
  (cond ((= times 0) true)
        ((slow-fermat-test n) (slow-prime? n (- times 1)))
        (else false)))

(define (slow-fermat-test n)
  (define (try-it a)
    (= (slow-expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


; So we'll run the same primes as before, with 1 Fermat check, and 
; compare the running-time of the two procedures.
; (As always, the tests are run independently from each other, since
; running them in the same environment seems to speed up subsequent
; tests and affects the results.)

;   n        |  Time (expmod)  |  Time (slow-expmod)  |  Ratio
; ----------------------------------------------------------------
; 1009       | 415             | 1313                 | 0.3161
; 1013       | 288             | 1338                 | 0.2152
; 1019       | 443             | 1353                 | 0.3274
; 10007      | 293             | 26907                | 0.0109
; 10009      | 280             | 15623                | 0.0179
; 10037      | 286             | 20287                | 0.0141
; 100003     | 435             | 127208               | 0.0034
; 100019     | 423             | 129240               | 0.0033
; 100043     | 327             | 127668               | 0.0026
; 1000003    | 386             | 1103592              | 0.0003
; 1000033    | 377             | 1102665              | 0.0003
; 1000037    | 351             | 1145410              | 0.0003

; As we can see, this newer expmod procedure definitely seems to slow
; things down. As hinted in the book, the expmod process with this new
; definition seems to have order of growth Theta(n) -- note how the time
; taken for a prime around 10,000 seems to be approximately 10 times longer
; than the time for a prime around 1000, and so on. 
;
; (Informally, we've got a nice "staircase" sort of shape happening in the 
; column with the results for the slow-expomod procedure, that exactly
; matches the shape in the column with the prime values.)


; So why is this happening? 
;
; As noted in the book, the original expmod procedure has order of growth
; Theta(log n), similar to the fast-expt procedure. Let's look at the
; procedure:

;;  (define (expmod base exp m)
;;    (cond ((= exp 0) 1)
;;          ((even? exp)
;;           (remainder (square (expmod base (/ exp 2) m))
;;                      m))
;;          (else
;;           (remainder (* base (expmod base (- exp 1) m))
;;                      m))))

; Recall that we've got applicative-order evaluation happening. So in the
; expression where we make the call to square, the argument (the recursive
; call to expmod) will be evaluated first, and then square gets called with
; the result. Note that the square procedure only has one parameter.


; Compare this to the slower expmod, with the call to square:

;;  (define (slow-expmod base exp m)
;;    (cond ((= exp 0) 1)
;;          ((even? exp)
;;           (remainder (* (slow-expmod base (/ exp 2) m)
;;                         (slow-expmod base (/ exp 2) m))
;;                      m))
;;          (else
;;           (remainder (* base (slow-expmod base (- exp 1) m))
;;                      m))))

; In the equivalent expression now, we've got a call to the * procedure
; instead, this time with two arguments (that evaluate to the same thing.)
; However, under the applicative-order evaluation all of the arguments are
; evaluated first. So it fires off a recursive call for the first argument,
; and once that's done, it fires off the second (redundant) recursive call
; to get the same answer again, before multiplying them together.


; So basically with the slower expmod procedure, we're doing the same work
; twice.
;
; Now the original expmod procedure generates a process with Theta(log n)
; order of growth, since we've halved the amount of work we need to do at
; each point by using that successive squaring trick. 
;
; However the slower expmod procedure does all the work of the original
; expmod procedure twice at each point, so we've exactly canceled out
; the gain we had.
;
; ie. we're doubling the amount of work at each point, so we have order 
; of growth Theta( 2^(log_2(n)) ) = Theta(n).
