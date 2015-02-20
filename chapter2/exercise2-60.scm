;;; SICP Exercise 2.60
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; In this exercise we modified our set data implementation to allow
;;; duplicate elements in the list, and compared the efficiency of each
;;; of the set operations.


; If we allow duplicates, then our element-of-set? procedure pretty
; much stays the same: we still want to be checking through the list
; until we find an element.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; Recall that before we had an order of growth Theta(n) for the
; number of steps for element-of-set? in a set with n elements.
;
; However, for the duplicate case, we don't actually have an upper
; bound on the number of elements in the list! We have at -least- n
; elements, but we could end up with a ridiculously long list here.

; eg. Consider the set {1,2} implemented as the ridiculous, yet plausible,
; list (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2).
;
; We're doing that much more work to check whether '2' is in the set. And
; let's not even get started on checking for elements that aren't in the set!

; So we actually can't even really comment on the order of growth in terms of
; Theta-notation with respect to the number of elements n in the set. **
;
; There is no useful relationship between the number of elements in the set
; and the number of elements in the list, which means that there's no
; relationship between the number of set elements n and the maximum number of
; steps our element-of-set? procedure would ever have to take.

; (** We can, of course, always talk about the efficiency in terms of the number
; of elements in the -list-. But that wouldn't give us a useful comparison in 
; terms of a set representation.)


; Our next procedure, adjoin-set, is much simpler this time around. We don't
; need to check that the element exists in the set; we just cons it on anyway.

(define (adjoin-set x set)
  (cons x set))

; So we have order of growth Theta(1) here: this is a constant-time operation,
; regardless of how big the set might be.


; Similarly, for union-set we simply append the two sets together:

(define (union-set set1 set2)
  (append set1 set2))

; However, the order of growth here is tricky, depending on how 'append' is
; defined. The book's definition for append was as follows:

;;  (define (append list1 list2)
;;    (if (null? list1)
;;        list2
;;        (cons (car list1) (append (cdr list1) list2))))

; So if we go by that definition, then notice that we're still cdr-ing down
; that entire first list -- which in our new implementation, can be 
; arbitrarily long with respect to the -actual- number of set elements n.
;
; This means that again, we can't actually make any real comment on the
; order of growth for the number of steps for the union-set procedure.


; Finally, for the intersection-set procedure, we don't really have any
; changes from the original definition: we still need to check that the
; element exists somewhere in both sets before we include it.

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; In terms of efficiency, this procedure truly is a mess. Not only are
; we cdr-ing down set1 again, which is already an operation with an 
; "unbounded" number of steps. But for -each- of those steps, we're 
; firing off a call to our element-of-set? procedure with set2, which 
; can also take an "unbounded" number of steps!


; In terms of practical application, there actually is a benefit here
; provided the lists are kept moderately reasonable. In such a case,
; our adjoin-set and union-set procedures are more efficient than in the 
; non-duplicate case.
;
; But note that there is nothing stopping those lists from expanding
; to ridiculous sizes if not kept in check, in which case all of the
; element-checking and union/intersection operations will be -less- efficient
; than they were before.