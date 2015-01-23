;;; SICP Exercise 2.22
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we explain the attempts to rewrite the
;;; square-list procedure of Exercise 2.21 in terms of an
;;; iterative process.


; So we're given the first attempt at a square-list procedure that
; generates an iterative process as follows:

(define (square x)
  (* x x))

(define (square-list-one items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

; But when we use this, we get the items in the reverse order:

(square-list-one (list 1 2 3 4 5))
;> {mcons 25 {mcons 16 {mcons 9 {mcons 4 {mcons 1 '()}}}}}

; The reason this happens is that the cons procedure constructs
; the list by putting the new item at the beginning, not the end.
; So our result list gets built up like this:
;   ()
;   (1)
;   (4 1)
;   (9 4 1)
;   ...

; By the end, we have the list of squares in reverse order.


; So an attempt to fix this is to interchange the arguments to
; cons, as follows:

(define (square-list-two items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))


; But when we run that, we still don't get what we want:

(square-list-two (list 1 2 3 4))
;> {mcons {mcons {mcons {mcons '() 1} 4} 9} 16}

; So recall that the cons procedure makes a cons cell with
; the first argument as the first part of the pair, and the
; second argument as the second part of the pair. 
;
; And this is all that is happening here, when we think 
; about it. Our "list" grows like this:
;   ()
;   ((), 1) 
;   (((), 1), 4)
;   ((((), 1), 4), 9)
;   (((((), 1), 4), 9), 16)
;
; Basically at each point we're taking the previous answer,
; sticking it in the car of the cons cell, and then putting
; the squared element in the cdr. So we get the structure that
; we see here happening.


; A simple fix would be to do what we did in Exercise 2.20, and
; -append- to the end of the list instead, like this:

(define (square-list-three items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))


; Then this procedure will work as we want it to:

(square-list-three (list 1 2 3 4 5))
;> {mcons 1 {mcons 4 {mcons 9 {mcons 16 {mcons 25 '()}}}}}