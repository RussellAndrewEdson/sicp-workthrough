;;; SICP Exercise 2.29
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us developing a data abstraction for a mobile
;;; (which has two branches that hang weights or other mobiles) using
;;; lists.

; We are given the constructor to make a mobile given its left and 
; right branches:

(define (make-mobile left right)
  (list left right))

; The constructor for a branch of the mobile is also given, which
; creates a branch out of a rod of the given length that has the
; given structure hanging from it:

(define (make-branch length structure)
  (list length structure))

; We can make an example mobile so that we can test out the 
; procedures as we write them:

(define odds (make-mobile (make-branch 1 3)
                          (make-branch 1 5)))
(define evens (make-mobile (make-branch 2 4)
                           (make-branch 2 6)))
(define mobile-one (make-mobile (make-branch 3 odds)
                                (make-branch 3 evens)))


; Part a)
; We want to write selectors for the left and right branches of
; a mobile. We can do that using our car and cdr procedures as
; follows (keeping in mind as always that the cdr returns us a
; pair with the empty list at the end, so we need to car after we
; cdr -- or cadr, to use the shorthand):

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

; We also want selectors for our branch representation that return
; us the length and structure of the branch. As the branch uses the
; same 2-element list implementation, we can use the exact same car
; and cadr operations that we had before.

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; We can test out our selectors on our example mobile:

(left-branch mobile-one)
;> {mcons 3 {mcons {mcons {mcons 1 {mcons 3 '()}} {mcons {mcons 1 {mcons 5 '()}} '()}} '()}}
; ie. (3 ((1 3) (1 5)))

(right-branch mobile-one)
;> {mcons 3 {mcons {mcons {mcons 2 {mcons 4 '()}} {mcons {mcons 2 {mcons 6 '()}} '()}} '()}}
; ie. (3 ((2 4) (2 6)))

(branch-length (left-branch mobile-one))
;> 3

(branch-structure (left-branch mobile-one))
;> {mcons {mcons 1 {mcons 3 '()}} {mcons {mcons 1 {mcons 5 '()}} '()}}
; ie. ((1 3) (1 5))

(branch-structure (right-branch (branch-structure (left-branch mobile-one))))
;> 5


; Part b)
; Next, we define a procedure total-weight that returns the total
; weight of a given mobile, but we want it to operate in terms of
; the selectors we defined above.

; Now the total weight of a mobile is the weight of its left branch
; plus the weight of its right branch. The weight of a branch is
; the total weight of the structure hanging on it (be it a simple
; weight or another mobile). The 'pair?' procedure will tell us 
; whether we have a simple weight or another mobile.

; (Of course, glancing ahead at part d), we might be inclined to
; at least isolate the 'pair?' operation in a separate procedure called
; 'mobile?' that allows us to operate at our higher level of abstraction.)

; So we can write that in a recursive way as follows:

(define (mobile? structure)
  (pair? structure))

(define (total-weight mobile)
  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (mobile? structure)
          (total-weight structure)
          structure)))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
  
; Let's test it with our example mobile (we should get 3+5+4+6=18):

(total-weight mobile-one)
;> 18


; Now we want to design a procedure to test whether a given mobile
; is balanced. We are given that the mobile is balanced when:
;   - the torque applied by its top-left branch is equal to the torque
;     applied by the top-right branch, where the torque is given by
;     the length of the branch rod multiplied by the total weight of
;     its structure;
;   - each of the submobiles hanging off the branches are themselves
;     balanced. We can instead say this as "each -substructure- is balanced",
;     where we automatically consider a simple weight structure to be
;     balanced -- this leads to a natural recursive definition.
;
; We can once again use our 'mobile?' procedure to prevent us from having
; 'pair?' appear anywhere in our code (which reveals our implementation
; details!)

; We can write that procedure as follows:

(define (balanced? mobile)
  (define (torque branch)
    (let ((structure (branch-structure branch)))
      (* (branch-length branch)
         (if (mobile? structure)
             (total-weight structure)
             structure))))
  (define (substructure-balanced? structure)
    (if (mobile? structure)
        (balanced? structure)
        true))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left)
            (torque right))
         (substructure-balanced? (branch-structure left))
         (substructure-balanced? (branch-structure right)))))

; Once again, we'll test this. Our example mobile is not balanced:

(balanced? mobile-one)
;> #f

; But the following mobile is balanced:

(define left-struct (make-mobile (make-branch 2 6)
                                 (make-branch 3 4)))
(define right-struct 5)
(define mobile-two (make-mobile (make-branch 1 left-struct)
                                (make-branch 2 right-struct)))

(balanced? mobile-two)
;> #t


; Part d)

; Now we have the following change to the constructors for the mobile:

;;  (define (make-mobile left right)
;;    (cons left right))

;;  (define (make-branch length structure)
;;    (cons length structure))

; To make our program work with the new representation, all we need to do is:
;   - modify our right-branch procedure to take the cdr instead of cadr,
;   - modify our branch-structure procedure to take the cdr instead of 
;     the cadr.
;
; And that's it. The rest of the code operates only in terms of those 
; abstractions, so it'll be fine with the change!
;
; (As a side note, we isolated that 'mobile?' procedure before but it turned
; out that we didn't need to. It was still worth it though. And of course, 
; we'd need to change that one too if we ever used something more exotic 
; than pairs/lists to represent our mobile data.)