;;; SICP Exercise 2.15
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we continued looking at the parallel-resistance
;;; equations and their evaluation by our interval arithmetic system.

; Our interval arithmetic system is as follows (along with the print
; method we had in Exercise 2.14):

(define (make-interval a b) (cons a b))

(define (upper-bound z) (cdr z))

(define (lower-bound z) (car z))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent center percentage)
  (let ((tolerance (* center percentage (/ 1 100))))
    (make-center-width center tolerance)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (interval-contains? x interval)
  (<= (lower-bound interval) x (upper-bound interval)))

(define (div-interval x y)
  (if (interval-contains? 0 y)
      (error "Interval contains zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (print-interval i)
  (newline)
  (display (center i))
  (display " ")
  (display (percent i))
  (display "%"))


; Continuing on from the discussion in Exercise 2.14, the suggestion is that
; we will get tighter error bounds if we can rewrite the expression we want
; to use so that no variable that represents the same uncertain number is
; repeated.

; This is actually correct! Let's see why.

; Suppose we have the simple interval (99, 101). Or 100 with a percentage
; tolerance of 1%. Let's call this interval A.

(define interval-a (make-interval 99 101))
(print-interval interval-a)
;> 100 1%


; Now suppose we want to evaluate 2A-A, which should give us back A exactly. 
; That makes sense, right? We're picking a number, multiplying
; it by 2 and then subtracting it from the result, which gets us right
; back where we started. This will work with any number in that interval,
; and we'll get back that exact number every time. Applied to all numbers in
; the interval simultaneously, we ought to get back the exact same interval.

; This also makes algebraic sense -- 2A-A = A algebraically, of course.

; And yet:

(define two (make-interval 2 2))
(print-interval (sub-interval (mul-interval two interval-a) interval-a))
;> 100 3%

; We've got an extra 2% error here!

; The problem is the same one as was hinted back at in Exercise 2.14. 
; The interval arithmetic system won't pick the same number; it'll pick
; different numbers from the same interval.

; eg. Where we were talking about, say, picking 99, multiplying it by 2
; to get 198 and then subtracting 99 to get back to 99, the system will
; pick 99, multiply it by 2 to get 198, and then instead subtract -any-
; number from A. In fact, we can get as far away as 101, so it can pick
; that -- it subtracts 101 from 198 to get 97, which is the minimum 
; possible value for this new interval.

; Repeating the same thing for the upper bound (pick 101, multiply by
; two to get 202, subtract a different value, 99, to get 103), and we
; actually end up with a minimum of 97 and a maximum of 103; or anything
; in between. So we have the interval (97, 103), or 100 with an error of 3%,
; as we saw in the computation.

; Of course, if we factor out the 'sameness' of A with algebraic manipulation,
; then we get our better result (in this case, 2A-A = A exactly, so we'll
; get the interval (99, 101) back, as we expect.

; So it stands to reason that we will always get tighter error bounds if
; we factor out the 'sameness' ourselves so that there are no repeated
; variables in our expression. Of course, we'd be right in suspecting that
; this isn't always possible to do.
