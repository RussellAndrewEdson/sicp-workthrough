;;; SICP Exercise 2.37
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we implemented some matrix algebra procedures
;;; using our accumulate and accumulate-n procedures.

; We have the accumulate and accumulate-n procedures from Exercise 2.36:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (first-elements seqs)
  (map car seqs))

(define (without-first-elements seqs)
  (map cdr seqs))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (first-elements seqs))
            (accumulate-n op init (without-first-elements seqs)))))


; And we are given the definition for the dot product, which uses the
; primitive map procedure that takes in an arbitrary number of sequences:

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


; Now for the matrix-*-vector procedure to give the vector result of
; multiplying a matrix by a vector, we can see from the formula that
; each element of the result vector will be the accumulated sum of
; multiplying corresponding elements of the vector v with the rows of
; the matrix m.
;
; Now this operation is simply the dot product we have above, so we
; just want to map the dot product (with v) across the rows of the
; matrix m!

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

; We can test this out:

(define mat-one (list (list 1 2) (list 3 4)))
(define vec-one (list 1 4))

(matrix-*-vector mat-one vec-one)
;> (9 19)


; Now for the transpose of the matrix m, we want to use our accumulate-n
; procedure to build the transpose by constructing the rows out of the
; columns of the matrix mat. Specifically, we want the first row to be
; the first column, which is actually the list of all of the first elements
; in the rows of m. And the second row is the list of all of the second
; elements, and so on.
;
; So this sounds pretty similar to how we actually designed the accumulate-n
; procedure in the first place! All we need to do then is accumulate the
; application of the -cons- procedure across mat, using the empty list as
; the initial value:

(define (transpose mat)
  (accumulate-n cons nil mat))

; We can show that this works:

(transpose (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
;> ((1 4 7) (2 5 8) (3 6 9))


; Then finally for our matrix-*-matrix procedure, we can define this in
; terms of our existing procedures! A nice way to multiply the matrices
; m and n is to "multiply by the rows" (there's an explanation of this
; up on MIT OpenCourseWare by the awesome Professor Gilbert Strang:
; http://ocw.mit.edu/courses/mathematics/18-06-linear-algebra-spring-2010/
; video-lectures/lecture-3-multiplication-and-inverse-matrices/ )
;
; In a nutshell, the rows of the result m*n will actually be the matrix n
; premultiplied by the corresponding row of m, which gives us the same 
; result as postmultiplying the row of m (as a vector) by the transpose of n!
;
; So to get the result vector, we simply map our matrix-*-vector procedure,
; that multiplies with the transpose of n (or cols, in our case), across the
; rows of m:

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

; And we can test this of course, to make sure that it works as expected:

(define mat-two (list (list 1 2) (list -1 3)))
(define mat-three (list (list 1 0) (list 2 -1)))

(matrix-*-matrix mat-two mat-three)
;> ((5 -2) (5 -3))