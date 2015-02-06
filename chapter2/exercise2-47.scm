;;; SICP Exercise 2.47
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we provided selectors 'origin-frame', 
;;; 'edge1-frame' and 'edge2-frame' for the given frame constructors.

; A first constructor for a frame is given as follows (we'll call this
; one frame-a, to distinguish it from the second constructor.)

(define (make-frame-a origin edge1 edge2)
  (list origin edge1 edge2))

; Then our selectors simply return the different elements of that list.
; 'origin-frame' returns the car of the frame list, 'edge1-frame' returns
; the cadr (ie, the car of the cdr), and 'edge2-frame' returns the caddr
; (ie. the car of the cdr of the cdr.)

(define (origin-frame-a frame)
  (car frame))

(define (edge1-frame-a frame)
  (cadr frame))

(define (edge2-frame-a frame)
  (caddr frame))

; A quick test (we'll use placeholder integers here, but in reality the
; selectors would return our vector representations.)

(define frame1 (make-frame-a 0 1 2))

(origin-frame-a frame1)
;> 0

(edge1-frame-a frame1)
;> 1

(edge2-frame-a frame1)
;> 2


; Our second frame constructor, which we'll call 'frame-b', is defined as:

(define (make-frame-b origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; This time, our origin is still the car of the frame and edge1 is still
; the cadr, but edge2 is now the cddr (ie. we don't need to "car" afterward.
; The cdr is a cons cell, and we simply take the cdr of that cell.)

(define (origin-frame-b frame)
  (car frame))

(define (edge1-frame-b frame)
  (cadr frame))

(define (edge2-frame-b frame)
  (cddr frame))

; We'll test this one too:

(define frame2 (make-frame-b 5 6 7))

(origin-frame-b frame2)
;> 5

(edge1-frame-b frame2)
;> 6

(edge2-frame-b frame2)
;> 7