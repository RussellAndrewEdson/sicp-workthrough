;;; SICP Exercise 2.2
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used the ideas of data abstraction to
;;; represent line segments in a plane, where lines are defined
;;; in terms of points, which are given a concrete implementation
;;; as a pair of x- and y-coordinates.

; So assume we have a representation of a point in the plane, so
; we can define a line segment in terms of its endpoints.
; Then our constructor and selectors for the line segment are:

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


; Now we can look at our implementation for a point, in terms of a pair
; of x- and y-coordinates:

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))


; At this point, we should test our procedures with the given print-point
; procedure:

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p1 (make-point -1 6))
(define p2 (make-point 2 -7))
(define p3 (make-point 2 -1))
(define p4 (make-point 7 7))

(define seg1 (make-segment p1 p3))
(define seg2 (make-segment p3 p4))

(print-point p1)
;> (-1,6)

(print-point p2)
;> (2,-7)

(print-point (start-segment seg2))
;> (2,-1)

(print-point (end-segment seg1))
;> (2,-1)


; So far so good. Now we can define our midpoint-segment procedure
; in terms of these abstractions:

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((x (average (x-point (start-segment segment))
                    (x-point (end-segment segment))))
        (y (average (y-point (start-segment segment))
                    (y-point (end-segment segment)))))
    (make-point x y)))


; And we can test our midpoint-segment procedure too.

; Midpoint of (-1,6)-(2,-1) should be (1/2, 5/2)
(print-point (midpoint-segment seg1))
;> (1/2,5/2)

; Midpoint of (2,-1)-(7,7) should be (9/2, 3)
(print-point (midpoint-segment seg2))
;> (9/2,3)
