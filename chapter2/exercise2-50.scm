;;; SICP Exercise 2.50
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we defined the 'flip-horiz' transformation to
;;; take a painter and flip it horizontally. We also defined transformations
;;; to rotate a given painter counterclockwise by 180 and 270 degrees.


; NOTE: The transform-painter procedure in the Soegaard SICP Picture 
; Language has a slightly different interface than the one in the book!
; Instead of taking the painter as an additional parameter, their
; procedure returns a lambda that takes the painter instead.
;
; eg. So instead of calling it like this:
;   (transform-painter painter
;                      origin
;                      edge1
;                      edge2)
;
; We call it like this:
;   ((transform-painter origin
;                       edge1
;                       edge2)
;    painter)
;
; Using the Soegaard interface allows us to easily display the pictures
; afterward, so my procedures will be written to work with it.


; Now the 'flip-horiz' transformation is similar to the 'flip-vert' one
; given in the book. Here we want to change our origin for the frame
; from the bottom-left corner to the bottom-right corner, and modify
; our edges accordingly.

; eg. We want the following:
;
;
;    (0,1) ---------- (1,1)         (0,1) ---------- (1,1)
;    edge2 |        |                     |        | edge2
;          |        |        --->         |        | 
;          |        |                     |        |
;    (0,0) ---------- (1,0)         (0,0) ---------- (1,0)
;    origin     edge1                     edge1      origin
;

; We can define this transformation as follows.
; (Note that we're using the Soegaard interface here, as explained
; above.)

(define (flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))


; For the rotation 180 degrees, we could actually just define it
; in terms of a -composition- of two 'rotate90' transformations. But
; where's the fun in that, right?
;
; So we'll do it the "long" way here. To rotate the picture 180 degrees
; counterclockwise, we want the origin to move to the top-right corner,
; and edge1 and edge2 move as shown in the diagram here.
;
;                                         edge1      origin
;    (0,1) ---------- (1,1)         (0,1) ---------- (1,1)
;    edge2 |        |                     |        | 
;          |        |        --->         |        | 
;          |        |                     |        | edge2
;    (0,0) ---------- (1,0)         (0,0) ---------- (1,0)
;    origin     edge1                           
;

; So our 'rotate180' procedure is as follows:

(define (rotate180 painter)
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))


; Similarly, our 270 degree rotation looks like this:
;
;                                   origin     edge2      
;    (0,1) ---------- (1,1)         (0,1) ---------- (1,1)
;    edge2 |        |                     |        | 
;          |        |        --->         |        | 
;          |        |               edge1 |        | 
;    (0,0) ---------- (1,0)         (0,0) ---------- (1,0)
;    origin     edge1                           
;

; Then our 'rotate270' procedure can be written like this.

(define (rotate270 painter)
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))


; Check out exercise2-50_test.png to see these procedures in action.