;;; SICP Exercise 1.44
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we wrote a procedure to smooth a given function,
;;; and then used our 'repeated' procedure from Exercise 1.43 to construct
;;; a procedure that generates the n-fold smoothed function.


; First let dx be some small number. 

(define dx 0.00001)


; Now the value of the smoothed function at a point x is the average of
; the values f(x-dx), f(x) and f(x+dx). We can write a procedure to 
; compute this function as follows:

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
                 3)))


; We can test it out with a "jumpy" function, like 1/sin(x) for values of
; x close to 0:

(define (one-on-sin x)
  (sin (/ 1 x)))

((smooth one-on-sin) 0.0002)
;> -0.835174011021902


; The results of the original function and the smoothed function are 
; tabulated side-by-side below for the range 0.0001 <= x <= 0.0010 :

;   x      |  1/sin(x)               |  smoothed 1/sin(x)
; --------------------------------------------------------------
; 0.0001   | -0.30561438888825215    | -0.13922089082976782
; 0.0002   | -0.9879664387667768     | -0.835174011021902
; 0.0003   | -0.10334303799242404    | -0.1535202980379771
; 0.0004   | -0.6501275235748956     | 0.2652365663278124
; 0.0005   | 0.930039504416137       | 0.13621420769128267
; 0.0006   | 0.9986605465280158      | -0.17939723748966085
; 0.0007   | 0.7534074467601115      | 0.2539843507193021
; 0.0008   | -0.3465363519963052     | -0.010606541571864659
; 0.0009   | -0.8482585627113032     | -0.7824154564751292
; 0.0010   | 0.8268795405320025      | -0.21569307507653876

; The results are not all that different (yet), but we can even see that
; the bounces aren't as pronounced in some points. In fact, all of the values
; 0.0004 <= x <= 0.0008 are much closer to zero with the smoothing than they
; were with the original function, as shown in the table.


; Now we can use this smooth procedure to calculate the n-fold smoothed
; function, using our repeated procedure from Exercise 1.43:

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (cond ((= n 0) identity)
        (else (lambda (x) 
                ((compose f (repeated f (- n 1))) x)))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

((n-fold-smooth one-on-sin 5) 0.0004)
;> -0.9253662487051465


; Let's try a 5-fold smooth on the 1/sin(x) function:

;   x      |  1/sin(x)               |  5-fold smooth: 1/sin(x)
; --------------------------------------------------------------
; 0.0001   | -0.30561438888825215    | -0.9153271756786214
; 0.0002   | -0.9879664387667768     | -0.8920342682923749
; 0.0003   | -0.10334303799242404    | 0.9113199740710062
; 0.0004   | -0.6501275235748956     | -0.9253662487051465
; 0.0005   | 0.930039504416137       | 0.901794129866925
; 0.0006   | 0.9986605465280158      | 0.9275987507405349
; 0.0007   | 0.7534074467601115      | -0.9235941427433189
; 0.0008   | -0.3465363519963052     | 0.9263143134273543
; 0.0009   | -0.8482585627113032     | -0.8877300882408349
; 0.0010   | 0.8268795405320025      | 0.8813978833636078

; Now we can see a more pronounced smoothing into a sort of square wave,
; with smaller differences between values on the same size of 0.
