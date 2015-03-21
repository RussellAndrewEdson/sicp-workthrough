;;; SICP Exercise 2.72
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we considered the order of growth in the number
;;; of steps needed to encode a symbol with the Huffman encoding scheme.


; For reference, recall that our encoding procedure looked like this:

;;  (define (encode-symbol symbol tree)
;;    (if (symbol-in-set? symbol (symbols tree))
;;        (if (leaf? tree)
;;            '()
;;            (let ((left-symbols (symbols (left-branch tree)))
;;                  (right-symbols (symbols (right-branch tree))))
;;              (cond ((symbol-in-set? symbol left-symbols)
;;                     (cons 0 (encode-symbol symbol (left-branch tree))))
;;                    ((symbol-in-set? symbol right-symbols)
;;                     (cons 1 (encode-symbol symbol (right-branch tree)))))))
;;        (error "symbol not in tree -- ENCODE-SYMBOL" symbol)))
;;
;;  (define (symbols tree)
;;    (if (leaf? tree)
;;        (list (symbol-leaf tree))
;;        (caddr tree)))
;;
;;  (define (symbol-in-set? x set)
;;    (cond ((null? set) false)
;;          ((eq? x (car set)) true)
;;          (else (symbol-in-set? x (cdr set)))))


; Some things to note right away:
;   1. Our 'symbol-in-set?' procedure has order of growth Theta(n), where
;      n is the number of symbols in the tree (since we simply cdr down that
;      symbols list.
;   2. The 'encode-symbol' procedure potentially makes 3 calls to that
;      symbol-in-set? procedure.
;   3. Because of the way we've set up the trees, the 'symbols' procedure
;      will basically list the symbols from left to right as they appear in
;      the tree.


; Let's simplify things to begin with though, and consider the case we had
; in Exercise 2.71, where we only ever had a single symbol on the left-branch
; at any point. This meant that we ended up with the most frequent symbol in
; the top left branch of the tree, and to get to the least-frequent symbol we
; needed to traverse n-1 right-branches down to the bottom of the tree.

; So since the 'symbols' procedure will literally list the symbols in order
; from most-frequent to least-frequent for this special case, we have that
; the 'symbol-in-set?' procedure will require exactly 1 call to determine
; that the most-frequent symbol exists, and n recursive calls to find the
; least-frequent symbol in the tree.


; So the order of growth of the number of steps needed to encode the most
; frequent symbol is given as follows:
;   - Our 'symbol-in-set?' check on the entire tree is effectively a Theta(1)
;     operation, since it will precisely be the first element.
;   - The second 'symbol-in-set?' check on the left-branch will return true
;     immediately (since we have a leaf containing exactly the most-frequent
;     symbol), so this is another Theta(1) operation.
;   - Our next recursive call returns the empty list, so we return the list
;     (0), and we're done already.
;
; For the most-frequent symbol, we have order of growth Theta(1) in the number
; of steps needed for the encoding (it will always take only the 3 steps as 
; detailed above, no matter how many symbols we have.)


; Now let's consider the least-frequent symbol encoding:
;   - The 'symbol-in-set?' check on the entire tree will end up looking at
;     every other symbol in the tree before it gets to the least-frequent
;     one. So this is a Theta(n) operation for sure.
;   - The second 'symbol-in-set?' check will always return false, but we only
;     ever have one element in the left-branch so this is a Theta(1) operation.
;   - The third 'symbol-in-set?' check will search through a list containing
;     all of the remaining symbols at each point (so we have symbol lists of
;     size n-1, n-2, n-3, ..., 2, 1 as we work our way down the tree). The
;     number of remaining elements can be as large as n-1, so we'll simplify 
;     things by just considering this to be a Theta(n) operation.
;   - That last check will always return true, so we always cons 1 on to the 
;     front of a list constructed from recursive calls. By the time we've
;     reached the least-frequent symbol, we've made n-1 recursive calls.

; So for the least-frequent symbol, the encoding procedure takes Theta(n) steps
; during each call, and ends up making about n recursive calls. So our order
; of growth in the number of steps needed to encode the least frequent symbol
; is Theta(n^2), where n is the number of symbols in the tree.


; So we have Theta(1) for the most-frequent symbol and Theta(n^2) for the least
; frequent symbol, at least for the special case we had in Exercise 2.71.

; As stated in the question, it is difficult to say much about the general case.
; But what we can notice about our special case is that the structure of the 
; tree actually represents the -worst case- for a Huffman encoding tree. As we
; only ever have one leaf in the left-branch at all times, our tree will 
; have the maximum possible depth, and so we actually know that no matter what
; our Huffman encoding tree looks like, our least-frequent element can be no
; further away than it appears here in the special case.

; So we can already say that the order of growth in the number of steps needed
; to encode the least-frequent symbol in an alphabet of n symbols can be no
; worse than order n^2, ie. O(n^2). (Note: -not- Theta(n^2), since we only have
; an upper bound here.)


; (In general, the shape of the encoding tree will have an impact on the 
; efficiency of the procedure. For instance, if we had a "balanced" tree like
; we saw in those set exercises a little while back, then our 'symbol-in-set?'
; procedure would only be looking at about half of the symbols in the (sub)tree
; at any point, so we'd almost have an order of growth Theta(log(n)). (Almost, 
; since the way that I've written the encoding procedure here means that we 
; still always have that Theta(n) search through the entire symbols list at 
; the start of the procedure.)