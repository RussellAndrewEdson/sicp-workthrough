;;; SICP Exercise 2.76
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we described the changes that need to be made
;;; to systems when new operations/types are added, for each of the
;;; strategies we've looked at (explicit dispatch, data-directed,
;;; and message passing.)

; For explicit dispatch (where we simply tag the data, and each operation
; checks the type to determine what it needs to do), the technique is not
; additive. In particular, if a new data type is added, we need to modify
; the existing operation procedures with a clause that lets them work with
; the new type, and we need to make sure that none of our procedures have
; the same name.
;
; Adding a new operation is also a bit of a hassle, since we would need to
; write up a procedure that conditionally checks all of the known types and
; dispatches accordingly. Depending on the number of types we have, this
; procedure could be massive and cumbersome (and hence we're more likely to
; make an error somewhere.)


; With the data-directed programming style, we have the table of operations.
; By defining packages for the different types and interfacing to the rest
; of the system only through adding entries in the operation table, we don't
; have to worry about procedure name conflicts anymore, for starters. And 
; this is also nice and additive: to introduce a new type, we simply create
; a new (encapsulated) package that contains the operations to work with
; that type, and add them to the table. Then all of the generic operations
; that work in terms of the table will automatically be able to work with
; the new type!
;
; If we want to add a new operation, the smoothest way to do this involves 
; modifying each package for the types and adding support for the new 
; operation. Of course, once that's done, we can simply write the generic 
; operation procedure to work in terms of the table, and we're fine.


; With message-passing, we have a similar sort of situation. Since each
; data object keeps track of exactly how it needs to act for any given
; operation, then adding support for a new operation is as simple as adding
; a clause to the dispatch procedure that defines the object. Then adding a 
; new type requires defining a new dispatch procedure with clauses for all 
; of the existing operations -- a similar load of work to what we had with 
; the data-driven style. The generic operation procedures will automatically
; work with the new type without any changes needed.


; So actually as it turns out, both the data-driven technique and the
; message-passing technique are pretty much equally suited to situations
; where new types or new operations are often added.
;
; When new types are often added, we just add new columns to the operations
; table in the data-driven case (none of the existing types/operation 
; code needs to be modified), and in the message-passing case, we simply
; define the new type and add clauses for each operation we want to support.
; In both cases, no existing code needs to be changed at all, and the generic
; procedures will work just fine with the new types.
;
; On the other hand, if new operations are often added, we need to add new
; rows to the operations table in the data-driven case (which involves
; updating each of the existing types to work with the new operation), and
; in the message-passing case we need to add a new clause to each existing
; data object (ie. update each of the existing types to work with the new
; operation). Either way, we end up modifying each of the existing types
; (by adding a clause/adding a type-specific operation to a package), so
; its the same amount of work.


; (Of course, we can note that in the message-passing case as we've used
; it, we can only support generic procedures of one argument. So this
; might be a factor we need to take into consideration too.)