;;; SICP Exercise 2.49
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we used the segments->painter procedure from the
;;; book to define some primitive painters.

; NOTE: For some reason I wasn't able to get the segments->painter
; procedure working with Neil Van Dyke's SICP support package (some sort
; of error with the for-each procedure "expecting a list" and yet having
; a problem with the list of segments I passed it...)
;
; So since I enjoyed testing the painters and looking at the pretty
; output pictures, I've gone ahead and imported the package for
; Mike Sperber and Jens Axel Soegaard's picture language directly:

#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; So for this exercise (and possibly some of the remaining picture
; language exercises), those of us using DrRacket will want to
; select 'Determine Language from Source'.


; Now we'll have access to the segments->painter procedure in the
; picture language implementation, but I'll include the book's version
; here for reference anyway:

;;  (define (segments->painter segment-list)
;;    (lambda (frame)
;;      (for-each
;;       (lambda (segment)
;;         (draw-line
;;          ((frame-coord-map frame) (start-segment segment))
;;          ((frame-coord-map frame) (end-segment segment))))
;;       segment-list)))


; a)
; So first, we want to define a painter that draws the outline of
; the designated frame. 
;
; Recall that we represent an image inside the [0,1)x[0,1) unit
; square. Note that 1.0 isn't actually included within our region!
; So we'll represent those edges of the frame with a sufficiently
; close value, like 0.99, instead.
; 
;    (0, 0.99) --------------- (0.99, 0.99)
;              |             |
;              |             |
;       (0, 0) --------------- (0.99, 0)

; So we'll define some constants for readability:

(define ymax 0.99)
(define ymin 0.0)
(define xmax 0.99)
(define xmin 0.0)


; Then our line segments for the original image are:
;   - the line segment from (xmin, ymin) to (xmin, ymax)
;   - the line segment from (xmin, ymax) to (xmax, ymax)
;   - the line segment from (xmax, ymax) to (xmax, ymin)
;   - the line segment from (xmax, ymin) to (xmin, ymin)
;
; (The order and direction of the segments doesn't matter, though.)

; So we can make these into 'top', 'left', 'right', 'bottom' segments:

(define top (make-segment (make-vect xmin ymax)
                          (make-vect xmax ymax)))
(define left (make-segment (make-vect xmin ymin)
                           (make-vect xmin ymax)))
(define right (make-segment (make-vect xmax ymin)
                            (make-vect xmax ymax)))
(define bottom (make-segment (make-vect xmin ymin)
                             (make-vect xmax ymin)))

; So we'll write this up as a painter procedure:

(define outline (segments->painter 
                 (list top left right bottom)))


; b)
; We can just as easily draw an 'X' by connecting opposite corners of
; the frame:
;
;         (xmin, ymax)  \   /  (xmax, ymax)
;                        \ /
;                        / \ 
;         (xmin, ymin)  /   \  (xmax, ymin)

(define x (segments->painter
           (list (make-segment (make-vect xmin ymin)
                               (make-vect xmax ymax))
                 (make-segment (make-vect xmin ymax)
                               (make-vect xmax ymin)))))


; c)
; To draw a diamond, we want to find the midpoint. To do that, 
; we want to find the averages of the x- and y-coordinates.
;
; Alternatively (if we're really lazy), we can recall that we did 
; this back in Exercise 2.02 already. So we can simply bring that code
; in (with some minor changes to match up with Soegaard's API):

(define (average x y)
  (/ (+ x y) 2))

(define (midpoint-segment segment)
  (let ((x (average (vector-xcor (segment-start segment))
                    (vector-xcor (segment-end segment))))
        (y (average (vector-ycor (segment-start segment))
                    (vector-ycor (segment-end segment)))))
    (make-vect x y)))

; Then we simply find the midpoints of the edges, and join
; them up:
;
;                        o  top-midpoint
;                      /   \
;     left-midpoint  o       o  right-midpoint
;                      \   /
;       bottom-midpoint  o

(define top-midpoint (midpoint-segment top))
(define left-midpoint (midpoint-segment left))
(define right-midpoint (midpoint-segment right))
(define bottom-midpoint (midpoint-segment bottom))

(define diamond (segments->painter
                 (list (make-segment left-midpoint top-midpoint)
                       (make-segment top-midpoint right-midpoint)
                       (make-segment right-midpoint bottom-midpoint)
                       (make-segment bottom-midpoint left-midpoint))))


; d)
; Finally, we want to implement that 'wave' painter from the book, which
; will be slightly more tricky.

; Luckily enough, in my hard copy of SICP the first frame from Figure 2.10
; is pretty much exactly 5cm x 5cm, which allows for some really accurate
; ruler measurements (since we want it to be -perfect-, of course!). Further,
; to get the coordinates for the vector, we simply need to determine the 
; position in centimetres from the lower left-hand corner, and then just
; double that number and divide by 10 to get the coordinates.

; Now in terms of the wave image itself, it actually consists of only
; 5 groups of connected segments, each of which we'll detail here:
; 
; left arm / head segments:
;   - point on the top edge 2.0cm in, ie. (0.40, ymax)
;   - point at (1.8cm, 4.2cm), ie. (0.36, 0.84)
;   - point at (2.0cm, 3.2cm), ie. (0.40, 0.64)
;   - point at (1.5cm, 3.2cm), ie. (0.30, 0.64)
;   - point at (0.8cm, 3.0cm), ie. (0.16, 0.60)
;   - point on the left edge 4.2cm up, ie. (xmin, 0.84)

(define left-arm-head (list (make-segment (make-vect 0.4 ymax)
                                          (make-vect 0.36 0.84))
                            (make-segment (make-vect 0.36 0.84)
                                          (make-vect 0.40 0.64))
                            (make-segment (make-vect 0.40 0.64)
                                          (make-vect 0.30 0.64))
                            (make-segment (make-vect 0.30 0.64)
                                          (make-vect 0.16 0.60))
                            (make-segment (make-vect 0.16 0.60)
                                          (make-vect xmin 0.84))))

; left arm / left leg segments:
;   - point on the left edge 3.2cm up, ie. (xmin, 0.64)
;   - point at (0.8cm, 2.0cm), ie. (0.16, 0.40)
;   - point at (1.5cm, 3.0cm), ie. (0.30, 0.60)
;   - point at (1.7cm, 2.4cm), ie. (0.34, 0.48)
;   - point on the bottom edge 1.2cm in, ie. (0.24, ymin)

(define left-arm-leg (list (make-segment (make-vect xmin 0.64)
                                         (make-vect 0.16 0.40))
                           (make-segment (make-vect 0.16 0.40)
                                         (make-vect 0.30 0.60))
                           (make-segment (make-vect 0.30 0.60)
                                         (make-vect 0.34 0.48))
                           (make-segment (make-vect 0.34 0.48)
                                         (make-vect 0.24 ymin))))

; left leg / right leg segments:
;   - point on the bottom edge 2.0cm in, ie. (0.40, ymin)
;   - point at (2.5cm, 1.5cm), ie. (0.50, 0.30)
;   - point on the bottom edge 3.0cm in, ie. (0.60, ymin)

(define left-right-leg (list (make-segment (make-vect 0.40 ymin)
                                           (make-vect 0.50 0.30))
                             (make-segment (make-vect 0.50 0.30)
                                           (make-vect 0.60 ymin))))

; right arm / head segments:
;   - point on the top edge 3.0cm in, ie. (0.60, ymax)
;   - point at (3.2cm, 4.2cm), ie. (0.64, 0.84)
;   - point at (3.0cm, 3.2cm), ie. (0.60, 0.64)
;   - point at (3.7cm, 3.2cm), ie. (0.74, 0.64)
;   - point on the right edge 1.8cm up, ie. (xmax, 0.36)

(define right-arm-head (list (make-segment (make-vect 0.6 ymax)
                                           (make-vect 0.64 0.84))
                             (make-segment (make-vect 0.64 0.84)
                                           (make-vect 0.60 0.64))
                             (make-segment (make-vect 0.60 0.64)
                                           (make-vect 0.74 0.64))
                             (make-segment (make-vect 0.74 0.64)
                                           (make-vect xmax 0.36))))

; right arm / right leg segments:
;   - point on the right edge 0.7cm up, ie. (xmax, 0.14)
;   - point at (3.0cm, 2.2cm), ie. (0.60, 0.44)
;   - point on the bottom edge 3.7cm in, ie. (0.74, ymin)

(define right-arm-leg (list (make-segment (make-vect xmax 0.14)
                                          (make-vect 0.60 0.44))
                            (make-segment (make-vect 0.60 0.44)
                                          (make-vect 0.74 ymin))))

; Then we simply append our segment groups together to get the full
; 'wave' picture.

(define wave (segments->painter (append left-arm-head
                                        left-arm-leg
                                        left-right-leg
                                        right-arm-head
                                        right-arm-leg)))

; And that's all there is to it!
; (Check out the exercise2-49_test.png to see those painters if you like.)