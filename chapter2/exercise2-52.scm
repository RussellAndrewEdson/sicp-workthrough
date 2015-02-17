;;; SICP Exercise 2.52
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we demonstrated working at the different levels
;;; of design (low-level, middle-level, top-level) by making changes to
;;; the "square-limit" of the wave painter.

; NOTE: We're using the Soegaard SICP Picture Language directly again,
; like we did in Exercise 2.49. So for those of us using DrRacket, make
; sure "Determine language from source" is enabled.

#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

; We'll also bring in each of the original procedures here. This will
; allow us to more easily see the differences when we make the changes
; for our new painter (and it'll also let us check out the differences 
; between the output pictures!)


; a)
; So first, we want to add some segments to the primitive wave painter
; that we had in Exercise 2.49. We'll bring in the code that we had
; before:

(define ymax 0.99)
(define ymin 0.0)
(define xmax 0.99)
(define xmin 0.0)

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

(define left-arm-leg (list (make-segment (make-vect xmin 0.64)
                                         (make-vect 0.16 0.40))
                           (make-segment (make-vect 0.16 0.40)
                                         (make-vect 0.30 0.60))
                           (make-segment (make-vect 0.30 0.60)
                                         (make-vect 0.34 0.48))
                           (make-segment (make-vect 0.34 0.48)
                                         (make-vect 0.24 ymin))))

(define left-right-leg (list (make-segment (make-vect 0.40 ymin)
                                           (make-vect 0.50 0.30))
                             (make-segment (make-vect 0.50 0.30)
                                           (make-vect 0.60 ymin))))

(define right-arm-head (list (make-segment (make-vect 0.6 ymax)
                                           (make-vect 0.64 0.84))
                             (make-segment (make-vect 0.64 0.84)
                                           (make-vect 0.60 0.64))
                             (make-segment (make-vect 0.60 0.64)
                                           (make-vect 0.74 0.64))
                             (make-segment (make-vect 0.74 0.64)
                                           (make-vect xmax 0.36))))

(define right-arm-leg (list (make-segment (make-vect xmax 0.14)
                                          (make-vect 0.60 0.44))
                            (make-segment (make-vect 0.60 0.44)
                                          (make-vect 0.74 ymin))))

(define wave (segments->painter (append left-arm-head
                                        left-arm-leg
                                        left-right-leg
                                        right-arm-head
                                        right-arm-leg)))


; Now let's make a quick and easily-noticeable change at this low-level.
; We'll define a new segment list for a triangle-shaped "smile" for
; the wave painter and add it in.

; Now our wave painter has the left and right "bends" in the sides of its 
; face at (1.8,4.2) cm and (3.2, 4.2) cm respectively, and its neck is
; determined by (2.0, 3.2) cm for the left and (3.0,3.2) cm for the right.
; So if we draw an upside-down triangle in that region, we should get
; something that doesn't look too creepy:

(define smile (list (make-segment (make-vect 0.42 0.76)
                                  (make-vect 0.58 0.76))
                    (make-segment (make-vect 0.58 0.76)
                                  (make-vect 0.50 0.68))
                    (make-segment (make-vect 0.50 0.68)
                                  (make-vect 0.42 0.76))))

(define wave2 (segments->painter (append left-arm-head
                                        left-arm-leg
                                        left-right-leg
                                        right-arm-head
                                        right-arm-leg
                                        smile)))

; And that should work: we should be able to see a "smile" on the wave figure
; when the picture gets painted.


; b)
; Let's work at the middle-level now. Now we had our original corner-split
; procedure defined as follows (along with the up-split and right-split
; procedures):

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


; This time around, let's change the pattern constructed by corner-split
; by using only one copy of the up-split and right-split images, not two.
; So our new corner-split operation is as follows:

(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split2 painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

; Note here that we've done away with the inner let structure (since
; we didn't need it to define top-left and bottom-right this time.)


; c)
; Finally, let's make a top-level change by modifying the square-limit
; pattern. 

; Now square-limit (and its square-of-four ancillary procedure) were
; originally defined as follows:

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


; So we'll modify the square-of-four procedure for our new picture
; by reversing the left-right directions for all of the combined painters
; (ie. in the book's square-limit example for Mr. Rogers in Figure 2.9, we
; want the big copy to face "outward" from each corner of the square 
; instead of inward. The effect of this will be that the smaller copies
; condense in a "+" cross shape in the middle of the frame, instead of 
; along the edges.
;
; So we can do this by simply rearranging where each of the rotations
; appears in the square-of-four parameter list, as shown.

(define (square-limit2 painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split2 painter n))))

; Also note that we're using our modified 'corner-split2' procedure here
; so that we can get the cumulative effect of all of our changes happening.


; And we're done! You can check out the comparison between the two 
; different output pictures in the file exercise2-52_test.png.