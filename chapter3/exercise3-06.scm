;;; SICP Exercise 3.6
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented a 'rand' procedure that
;;; could be reset to a particular value.


; First, we're going to need some sort of update procedure
; to produce the random numbers in the sequence. We can take
; the advice of one of the footnotes in the book and define
; a 'rand-update' procedure that uses modular arithmetic 
; parameterized by integers a, b, and m. (We'll pick these
; integers to be unrealistically small, just to keep things
; simple.)

(define (rand-update x)
  (let ((a 3) (b 7) (m 31))
    (modulo (+ (* a x) b) m)))

; This procedure can be applied to a given starting value to
; get a "random" sequence of numbers:

(rand-update 5)
;> 22

(rand-update 22)
;> 11

(rand-update 11)
;> 9

(rand-update 9)
;> 3
; ...etc.


; Then we simply construct our 'rand' procedure to use this
; update procedure with an internal variable to generate the
; random sequence.
;
; Now we also want our procedure to be able to reset the internal
; variable to a given value. We do so when the rand procedure
; is passed the symbol 'reset. If instead the procedure is passed  
; the symbol 'generate we update the variable to the next random
; number in the sequence instead (we'll set the initial random 
; number to 0 arbitrarily to begin with). And we'll signal an error 
; for any other inputs.
;
; So we would write the procedure as follows. Notice that we've
; defined an internal procedure 'update-and-return' -- this is simply
; to avoid the code duplication and (begin ..) structures that are
; otherwise needed on both branches of the conditional.

(define rand
  (let ((current-value 0))
    (lambda (action)
      (define (update-and-return value)
        (set! current-value value)
        current-value)
      (cond ((eq? action 'reset)
             (lambda (new-value)
               (update-and-return new-value)))
            ((eq? action 'generate)
             (update-and-return (rand-update current-value)))
            (else (error "Unknown action -- RAND" action))))))


; And that's it! Now we can generate "random" sequences, and in
; particular, we can repeat sequences by resetting the value 
; appropriately.

; We can test this out using the following procedure that creates
; a sequence of n random numbers using the 'rand' procedure:

(define (generate-random-sequence n initial-value)
  (define (iter count sequence)
    (if (= count 0)
        sequence
        (iter (- count 1) (append sequence (list (rand 'generate))))))
  (iter (- n 1) (list ((rand 'reset) initial-value))))

(generate-random-sequence 15 7)
;> (7 28 29 1 10 6 25 20 5 22 11 9 3 16 24)

(generate-random-sequence 13 29)
;> (29 1 10 6 25 20 5 22 11 9 3 16 24)
; Notice that we have the same numbers from 29 onwards in both sequences.