;;; SICP Exercise 1.36
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we wanted to modify the fixed-point procedure to make
;;; it print the sequence of approximations to the screen as it goes. We then
;;; used the procedure to find the fixed point of x -> log(1000)/log(x).


; To get the fixed-point procedure to output its intermediate results, we
; can use the newline and display primitives (that we used in Exercise 1.22).
;
; We add a 'print-result' procedure to encapsulate the output process. This
; procedure is then called at the start of each iteration of the fixed-point
; method.

(define tolerance 0.00001)

(define (print-result result)
  (display result)
  (newline))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (print-result guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; As we can see, the fixed-point procedure then outputs the first
; guess, and every other intermediate guess before the final answer.
; Note that it doesn't actually print the final result, though it
; does appear in the REPL as always. I've marked the return output
; (which doesn't get printed by our procedure) with a **.

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;> 1.0
;> 2.0
;> 1.5
;> 1.6666666666666665
;> 1.6
;> 1.625
;> 1.6153846153846154
;> 1.619047619047619
;> 1.6176470588235294
;> 1.6181818181818182
;> 1.6179775280898876
;> 1.6180555555555556
;> 1.6180257510729614
;> 1.6180371352785146
;> 1.6180327868852458  **


; Now we want to find a solution to x^x = 1000, by using our procedure
; to find the fixed-point of the transformation x -> log(1000)/log(x).
; We'll do so with and without average damping.

; Without average damping is a fairly straight-forward application.
; Of course, we can't use a guess of 1.0 anymore since we'd end up 
; dividing by zero (log(1) =0), so we can pick the guess 1.5 instead:

(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.5)


; Let's try -with- average damping next. To stop the guess from changing
; as much between the oscillations of the procedure, we can modify the
; transformation to take the average of x and the new value first:

(define (average x y) (/ (+ x y) 2))

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 1.5)


; The results of both of the methods are included here for
; side-by-side comparison. (The return value is marked with a ** again.)

;  iteration  | guess (w/o damping)      | guess (w/ damping) 
; ----------------------------------------------------------------
; 01          | 1.5                      | 1.5
; 02          | 17.036620761802716       | 9.268310380901358
; 03          | 2.436284152826871        | 6.185343522487719
; 04          | 7.7573914048784065       | 4.988133688461795
; 05          | 3.3718636013068974       | 4.643254620420954
; 06          | 5.683217478018266        | 4.571101497091747
; 07          | 3.97564638093712         | 4.5582061760763715
; 08          | 5.004940305230897        | 4.555990975858476
; 09          | 4.2893976408423535       | 4.555613236666653
; 10          | 4.743860707684508        | 4.555548906156018
; 11          | 4.437003894526853        | 4.555537952796512
; 12          | 4.6361416205906485       | 4.555536087870658 **
; 13          | 4.503444951269147        |
; 14          | 4.590350549476868        |
; 15          | 4.532777517802648        |
; 16          | 4.570631779772813        |
; 17          | 4.545618222336422        |
; 18          | 4.562092653795064        |
; 19          | 4.551218723744055        |
; 20          | 4.558385805707352        |
; 21          | 4.553657479516671        |
; 22          | 4.55677495241968         |
; 23          | 4.554718702465183        |
; 24          | 4.556074615314888        |
; 25          | 4.555180352768613        |
; 26          | 4.555770074687025        |
; 27          | 4.555381152108018        |
; 28          | 4.555637634081652        |
; 29          | 4.555468486740348        |
; 30          | 4.555580035270157        |
; 31          | 4.555506470667713        |
; 32          | 4.555554984963888        |
; 33          | 4.5555229906097905       |
; 34          | 4.555544090254035        |
; 35          | 4.555530175417048        |
; 36          | 4.555539351985717  **    |


; As you can see, applying the average damping has resulted in a dramatically
; quicker convergence in this case; returning an answer within the desired
; tolerance with only a third of the steps required!