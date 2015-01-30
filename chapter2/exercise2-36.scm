;;; SICP Exercise 2.36
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; This exercise had us completing the definition for the accumulate-n
;;; procedure that generalised our previous accumulate procedure to
;;; accumulate an arbitrary number of sequences.

; We have our accumulate procedure as always:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


; Now we are given the partial definition for the accumulate-n
; procedure here:

;;  (define (accumulate-n op init seqs)
;;    (if (null? (car seqs))
;;        nil
;;        (cons (accumulate op init <??>)
;;              (accumulate-n op init <??>))))

; Now we want the procedure to accumulate all of the first elements, 
; all of the second elements, and so on. So we would like a few 
; ancillary procedures to: 1. isolate all of the first elements, and 
; 2. isolate the sequences without their first elements.

; Fortunately, this sounds exactly like we're just mapping the
; car and cdr procedures across the seqs list each time! So we'll
; do exactly that:

(define (first-elements seqs)
  (map car seqs))

(define (without-first-elements seqs)
  (map cdr seqs))

; Then we can fill in the blanks with exactly these procedures to 
; get what we need. The existing accumulate-n code will check when
; we're done (by making sure the first list isn't empty at any
; point), and our accumulate-n procedure kind of mirrors our existing
; accumulate procedure in how we end up cdring down to the last element
; first, and then build our way back up.

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (first-elements seqs))
            (accumulate-n op init (without-first-elements seqs)))))


; We can test our accumulate-n procedure with the given example from
; the book:

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(accumulate-n + 0 s)
;> (22 26 30)