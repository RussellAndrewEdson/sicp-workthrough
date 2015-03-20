;;; SICP Exercise 2.71
;;; (The online text can be found at 
;;;     http://mitpress.mit.edu/sicp/full-text/book/book.html)
;;;
;;; For this exercise, we looked at some Huffman encoding trees for n
;;; symbols with relative frequencies 1,2,4,8,...,2^(n-1), and determined
;;; the number of bits needed to encode the least/most frequent symbols.


; Suppose we have a Huffman encoding tree with n symbols (x1,x2,...,xn)
; where the relative frequencies of the symbols are 1, 2, 4, 8, ..., 2^(n-1)
; (ie. so the symbol xi has frequency 2^(i-1). )

; Then we can notice that at all times during that leaf-merging process
; to create the tree, the sum of the 2 elements with the least weight
; will always be less (exactly 1 less, in fact) than the weight of the
; next (ie. 3rd) smallest element.
;
; For instance, if we had the symbols with weights 1, 2, 4, 8, 16, then:
;   1. The sum of the weights 1, 2 is 3, which is less than 4.
;   2. When we merge those elements for the new list, the two smallest
;      elements will have weights 3 and 4, which sum to 7, less than 8.
;   3. Merging those elements in the new list, the two smallest weight
;      elements are those with weights 7 and 8, which sum to 15, less
;      than 16, and so on.
;
; As this is the case, then we will have a very "linear" merging process
; as we build up the tree, where we only ever place one element on the
; left branch of a subtree:
;   (x2 x1)
;   (x3 (x2 x1))
;   (x4 (x3 (x2 x1)))
;   (x5 (x4 (x3 (x2 x1))))
;   ... and so on.

; The sketches for the Huffman encoding trees for n=5 symbols and n=10
; symbols are shown in the exercise2-71_trees.png file.


; So for such a tree with n symbols, we can notice that the depth of the
; tree will always be n-1 (because of the fact that we are only placing
; one element in the left branch at each point).
;
; Then we will always require 1 bit to encode the most frequent symbol
; (it appears in the top-left branch of the tree), and n-1 bits to
; encode the least frequent symbol (which always appears on the 
; right-hand branch at the bottom.)