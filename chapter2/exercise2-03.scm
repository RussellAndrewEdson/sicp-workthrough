;;; SICP Exercise 2.3
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we looked at making some different representations
;;; for a rectangle in the plane, where they both work interchangably
;;; with procedures for computing the perimeter and area.


; Now we can completely define a rectangle in the plane in terms of the
; location of one of its corner points (the top-left corner is normally the
; one we pick), and the rectangle's width and height.
;
; So assume for a second that we have the constructor make-rect and the 
; selectors width-rect and height-rect (we won't actually need the position
; of the rectangle's top-left corner here, but you can see that we 
; could easily set it up if we wanted). Then we can define the procedures 
; to compute the area and perimeter as follows:

(define (perimeter rect)
  (+ (* 2 (width-rect rect))
     (* 2 (height-rect rect))))

(define (area rect)
  (* (width-rect rect) (height-rect rect)))


; Now all we need is a rectangle implementation. As per the hint, we'll
; bring in some of the code from Exercise 2.2; in particular, our
; code for constructing points and line segments.

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

; In terms of our points, we'll also define an auxiliary procedure that
; allows us to calculate the horizontal and vertical distance between a
; pair of points -- this will prove useful in both of our representations.

(define (horizontal-distance-between-points p1 p2)
  (abs (- (x-point p2) (x-point p1))))

(define (vertical-distance-between-points p1 p2)
  (abs (- (y-point p2) (y-point p1))))


; Now for our first implementation, we could define a rectangle in terms
; of two orthogonal sides, which will be line-segments as we've defined 
; them above. So our first representation for a rectangle will be a cons
; cell containing these two sides, and the selectors are then defined as
; follows:

(define (make-rect horizontal-side vertical-side)
  (cons horizontal-side vertical-side))

(define (width-rect rect)
  (let ((side (car rect)))
    (horizontal-distance-between-points (start-segment side)
                                        (end-segment side))))
    
(define (height-rect rect)
  (let ((side (cdr rect)))
    (vertical-distance-between-points (start-segment side)
                                      (end-segment side))))


; We can test our implementation as follows:

; an 8x4 rectangle in the plane..
(define h-side (make-segment (make-point -2 3) (make-point 6 3)))
(define v-side (make-segment (make-point -2 3) (make-point -2 -1)))

(define rectangle (make-rect h-side v-side))

(perimeter rectangle)
;> 24

(area rectangle)
;> 32

; So our implementation works!


; For our second implementation, we can always define a rectangle
; by its diagonal points (say the upper-left and bottom-right points).
; So we'll do that here.
;
; Note: I've changed the perimeter and area procedures here so they don't
; clash with the ones above, but remember that the whole point is that
; the implementations are interchangable. So in reality the procedures
; have the same names, and you can drop-in replace either implementation
; with no extra effort.

(define (perimeter2 rect)
  (+ (* 2 (width-rect2 rect))
     (* 2 (height-rect2 rect))))

(define (area2 rect)
  (* (width-rect2 rect) (height-rect2 rect)))

(define (make-rect2 top-left-corner bottom-right-corner)
  (cons top-left-corner bottom-right-corner))

(define (width-rect2 rect)
  (horizontal-distance-between-points (car rect) (cdr rect)))

(define (height-rect2 rect)
  (vertical-distance-between-points (car rect) (cdr rect)))


; And our new implementation works just like the old one as far
; as the perimeter and area procedures are concerned.

; the same 8x4 rectangle in the plane..
(define top-left (make-point -2 3))
(define bottom-right (make-point 6 -1))

(define rectangle2 (make-rect2 top-left bottom-right))

(perimeter2 rectangle2)
;> 24

(area2 rectangle2)
;> 32
