;;; SICP Exercise 2.51
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we defined the 'below' operation for the
;;; painters, where it takes two painters as arguments and
;;; displays the first painter -below- the second painter.


; NOTE: Recall that the transform-painter procedure included
; in the Soegaard SICP Picture Language has a slightly different
; interface, as pointed out in Exercise 2.50.


; Now we'll define our 'below' procdure twice. First, we'll define
; it in terms of the frames with their origins and edges.
;
; What we want is the following: we want to display the first
; painter in a frame that starts at the origin but only has its
; vertical edge move up to half of the full square, and we want
; to display the second painter with its origin at that same
; halfway point.
;
; That is, we want the following:
;
;
;        (0,1) ----------------------------- (1,1)
;              | edge2                     |
;              |        PAINTER 2          |
;              |                           |
;       origin |                     edge1 |
;      (0,0.5) |---------------------------| (1,0.5)
;              | edge2                     |
;              |                           |
;              |        PAINTER 1          |
;              |                     edge1 |
;        (0,0) ----------------------------- (1,0)
;       origin 
;

; So we can define this operation in a similar way to the 
; 'beside' operation from the book, as follows.
; (We'll call this procedure 'below1').

(define (below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)
            painter1))
          (paint-top
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))
            painter2)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))


; However, given that we have the 'beside' operation and the
; rotation transformations from Exercise 2.50 (which already
; exist in the Soegaard SICP Picture Language anyway), notice that
; we can also define a 'below' procedure by working through it
; like this:

; Consider what the painter looks like when we're done, ie.
;
;              ----------------------------- 
;              | edge2                     |
;              |        PAINTER 2          |
;              |                           |
;              |                     edge1 |
;      origin  |---------------------------| 
;              | edge2                     |
;              |                           |
;              |        PAINTER 1          |
;              |                     edge1 |
;      origin  ----------------------------- 
;
;
; Suppose we rotated that entire picture 90 degrees -clockwise-.
; We'd have the following:
;
;          origin           origin
;              ----------------------------- 
;              |      edge2  |      edge2  |
;              |             |             |
;              |             |             |
;              |             |             |
;              |   PAINTER   |   PAINTER   | 
;              |      1      |      2      |
;              |             |             |
;              |             |             |
;              | edge1       | edge1       |
;              ----------------------------- 
;
; Then we have a 'beside' operation here, with component
; painters that look like this:
;
;                                     edge2
;       origin ----------------------------- 
;              |                           |
;              |                           |
;              |                           |
;              |         PAINTER           |
;              |           1/2             | 
;              |                           |
;              |                           |
;              |                           |
;       edge1  |                           |
;              ----------------------------- 
;
; And we can get back to the original painters by simply rotating
; 90 degrees counter-clockwise!
;                                  
;              ----------------------------- 
;        edge2 |                           |
;              |                           |
;              |                           |
;              |         PAINTER           |
;              |           1/2             | 
;              |                           |
;              |                           |
;              |                           |
;              |                           |
;       origin ----------------------------- 
;                                     edge1
;
; So for our procedure, we should simply work through those operations
; in reverse. That is:
;   1/ Rotate both Painters 1 and 2 clockwise by 90 degrees (ie.
;      counter-clockwise by 270 degrees, using our rotate270 procedure
;      from Exercise 2.50
;   
;   2/ Place Painter 1 on the left of Painter 2 using the beside operation.
;
;   3/ Rotate the entire picture counter-clockwise by 90 degrees (using
;      the rotate90 procedure from the book.

; So our second 'below' procedure, below2, can be defined as follows.

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))


; And that's it! See the other file exercise2-51_test.png for a demonstration
; that our below1, below2 operations work.