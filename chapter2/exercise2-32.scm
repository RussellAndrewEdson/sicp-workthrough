;;; SICP Exercise 2.32
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we completed the definition for given procedure
;;; to generate the power set (ie. set of all subsets) of a set, and
;;; explained how it worked.

; So we want a procedure that takes a set:
;   (1 2 3)
; and returns the power set:
;   (() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))

; Now you'll notice that the output of the subsets in the book is
; in a slightly different order:
;   (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; This immediately gives us a hint at what might be going on. As we
; add subsets from left to right, it seems like we're doing the following:
;   We start with the empty set (), as always. We add it to our subset list.
;   Let S denote the set of elements we're looking at, eg. S = (1 2 3).
;
;   1. We start at the back of the set S, and select that element.
;   2. Then we iterate through our list of subsets, adding this element to
;      each of them to create new subsets -- which we append to the list.
;   3. We then remove that element from S, and repeat the process until we've
;      exhausted all of the elements.

; So our subset list evolves in the following way:
;   (() )
;   (() ) + ((3))
;   (() (3)) + ((2) (2 3))
;   (() (3) (2) (2 3)) + ((1) (1 3) (2 3) (1 2 3))

; And if we look at the partial procedure definition that is given to us, 
; this appears to be exactly what's happening:

;;  (define (subsets s)
;;    (if (null? s)
;;        (list nil)
;;        (let ((rest (subsets (cdr s))))
;;          (append rest (map <??> rest)))))


; Let's consider a short example set, (1 2), to see how this works and what
; we want to happen at the <??> point. When we call (subsets '(1 2)), the let
; statement recursively calls (subsets '(2)), which recursively calls 
; (subsets '()). At that point, our set is null, so we return with the list
; containing nil, ie. (() ).
;
; At that point, we're back in the 1st recursive call where s is the set (2) 
; and we just bound rest to the set (() ). 
;
; So what we want to do is append rest with the result of "adding" the car 
; of our set s, which is 2, to every subset in the rest list. This should
; get us where we want to go!
;
; Assuming we've accomplished that somehow, then we'll have the list (() (2)),
; and we return that to the upper level recursive call, where s is (1 2) and
; the variable rest is bound to (() (2)). Then if we append rest to our
; result of adding the car of s, 1, to the front of each subset in the rest
; list, we will get (() (2) (1) (1 2)), which is exactly what we'd want!
; 
; We'd then return that list as the value of the entire procedure, and we'd 
; have the power set exactly.

; So at the point <??>, we want to have a procedure that takes in a set, and
; returns the result of consing the car of the set s to that set. We can even
; write this as a closure when we define the subsets procedure:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (subset) (cons (car s) subset))
                          rest)))))

; We can test this with some sets, and see that we get the power set
; every time:

(subsets (list 1 2))
;> (() (2) (1) (1 2))

(subsets (list 1 2 3))
;> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

(subsets (list 1 2 3 4))
;> (() (4) (3) (3 4) (2) (2 4) (2 3) (2 3 4) (1) (1 4) (1 3) ..
;>  .. (1 3 4) (1 2) (1 2 4) (1 2 3) (1 2 3 4))
