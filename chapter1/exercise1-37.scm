;;; SICP Exercise 1.37
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we createsd a procedure to compute the value
;;; of a k-term finite continued fraction given procedures n and d
;;; that generate the individual terms. We then used the procedure to
;;; approximate the reciprocol of the golden ratio.


; So we want a procedure to generate the truncated continued fraction.
; These will look like the following:

; 1-term:  n1/d1,                  or (/ n1 d1)
; 2-term:  n1/(d1+(n2/d2)),        or (/ n1 (+ d1 (/ n2 d2)))
; 3-term:  n1/(d1+n2/(d2+n3/d3))), or (/ n1 (+ d1 (/ n2 (+ d2 (/ n3 d3)))))

; We can see an easy pattern forming in the expressions on the
; right-hand side. Then we can express the k-th term as follows:

; k-term: (/ n1 (+ d1 (/ n2 (+ d2  ... (/ nk dk)))))


; Using this, we can write our procedure cont-frac to generate the 
; k-term continued fraction using an iterative process as follows:

(define (cont-frac n d k)
  (define (frac-iter n d count frac)
    (if (= count 0)
        frac
        (frac-iter n
                   d
                   (- count 1)
                   (/ (n k) (+ (d k) frac)))))
  (frac-iter n d k 0))


; We can test this with some small values to make sure that it works:

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 1)
;> 1.0
; 1/1 = 1.0

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 2)
;> 0.5
; 1/(1+(1/1)) = 1/2 = 0.5

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3)
;> 0.6666666666666666
; 1/(1+(1/(1+(1/1))) = 1/(1+(1/2)) = (1/(3/2)) = 2/3 = 0.6666...

; Sure enough, our procedure seems correct!


; So let's use this procedure to approximate the golden ratio
; to 4 decimal places.

; Recall that the golden ratio is (1+ sqrt(5))/2:

(define golden-ratio-reciprocol (/ 2 (+ 1 (sqrt 5))))
;> 0.6180339887498948


; How large do we need k to be so that the k-term that we saw
; above approximates this value? We can write a procedure to figure
; this out (and print the intermediate results too):

(define (print-result iter result)
  (display iter)
  (display "  ")
  (display result)
  (newline))

(define (golden-ratio-approximation)
  (define (within-4dp? result)
  (< (abs (- golden-ratio-reciprocol result))
     0.00005))
  (define (approx-iter k)
    (let ((k-term (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))
      (print-result k k-term)
      (if (not (within-4dp? k-term))
          (approx-iter (+ k 1)))))
  (approx-iter 1))


; Running this procedure gives us the following result:

(golden-ratio-approximation)
;> 1  1.0
;> 2  0.5
;> 3  0.6666666666666666
;> 4  0.6000000000000001
;> 5  0.625
;> 6  0.6153846153846154
;> 7  0.6190476190476191
;> 8  0.6176470588235294
;> 9  0.6181818181818182
;> 10  0.6179775280898876
;> 11  0.6180555555555556

; So the 11-term approximates the reciprocol of the golden ratio
; to 4 decimal places.


; We could also have written cont-frac as a recursive process, like this:

(define (cont-frac-recursive n d k)
  (define (next-term count)
    (cond ((= count k) (/ (n k) (d k)))
          (else (/ (n k)
                   (+ (d k) (next-term (+ count 1)))))))
  (next-term 1))

; Note the delayed operations here, which will give us our recursive
; process.

; This procedure works like the other one:

(cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 2)
;> 0.5

(cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 3)
;> 0.6666666666666666

(cont-frac-recursive (lambda (i) 1.0) (lambda (i) 1.0) 11)
;> 0.6180555555555556