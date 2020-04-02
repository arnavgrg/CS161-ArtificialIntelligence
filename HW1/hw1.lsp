
#|Q1.
Essentially, ordered tree traversal in a binary-search style. If N = m, 
where m is the middle element in [L m R], then return true. Otherwise, 
if N > m, recursively search in R, otherwise recursively search in L.
Test Cases:
    (print (TREE-CONTAINS 5 12)) -> Nil
    (print (TREE-CONTAINS 3 '((1 2 3) 7 8))) -> T
    (print (TREE-CONTAINS 4 '((1 2 3) 7 8))) -> Nil
    (print (TREE-CONTAINS 6 '((1 2 3) 6 (7 8)))) -> T
|#

(defun TREE-CONTAINS (N TREE)
    (cond
        ((null TREE) nil)
        ((atom TREE) (eql N TREE))
        ((eql N (second TREE)) t)
        ((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
        ((< N (second TREE)) (TREE-CONTAINS N (first TREE)))
    )
)

(print (TREE-CONTAINS 5 12))
(print (TREE-CONTAINS 3 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 4 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 6 '((1 2 3) 6 (7 8))))

;-------------------------------------------------------------------------------

#|Q2.
Since it's an ordered tree, we can make use of the property that the smallest 
element will be the first element in the tree. However, we must keep recursing
within the nested list until we reach an atom for the first element, like in
(((1) 2 3) 7 8), which becomes an interesting edge case. 
Test Cases:
    (print (TREE-MIN '(2 (3 4 5) 7 8))) -> 2
    (print (TREE-MIN '((1 2 3) 7 8))) -> 1
    (print (TREE-MIN '(((1) 2 3) 7 8))) -> 1
|#

(defun TREE-MIN (TREE)
    (cond 
        ((null TREE) nil)
        ((atom (car TREE)) (car TREE))
        (t (TREE-MIN (car TREE)))
    )
)

(TERPRI)
(print (TREE-MIN '(2 3 (4 5 7 8))))
(print (TREE-MIN '((1 2 3) 7 8)))
(print (TREE-MIN '(((1) 2 3) 7 8)))

;-------------------------------------------------------------------------------

#|Q3.
Pre-order traversal for a tree is typically root, left, right. Therefore, 
since our ordered tree is either a single root node, or arranged using [L m R], 
we can simply replicate this idea to get a pre-order traversal.
Append -> ((1 2 3) (4 5)) -> (1 2 3 4 5)
Cons -> (cons 'a (cons 'b nil)) -> (a b)
List -> (list 'a 'b) -> (a b)
Test Cases:
    (print (TREE-ORDER 3)) -> (3)
    (print (TREE-ORDER '((1 2 3) 7 8))) -> (7 2 1 3 8)
    (print (TREE-ORDER '((1 2 3) 6 ((8 9 10) 11 12)))) -> (6 2 1 3 11 9 8 10 12)
|#

(defun TREE-ORDER (TREE)
    (cond 
        ((null TREE) nil)
        ((atom TREE) (list TREE))
        ((append (cons (second TREE) (TREE-ORDER (first TREE))) 
                 (TREE-ORDER (third TREE))))
    )
)

(TERPRI)
(print (TREE-ORDER 3))
(print (TREE-ORDER '((1 2 3) 7 8)))
(print (TREE-ORDER '((1 2 3) 6 ((8 9 10) 11 12))))

;-------------------------------------------------------------------------------

#|Q4.
Since we need to manipulate a single list, we must the property of cons, that is,
conditions are executed in sequence. Therefore, we can incrementally remove elements
from the list until we hit the start index. From there, we can move on to the next
conditional and incrementally build a list of elements of length len and return that.
Test Cases:
    (print (SUB-LIST '(a b c d) 0 3)) -> (a b c)
    (print (SUB-LIST '(a b c d) 3 1)) -> (d)
    (print (SUB-LIST '(a b c d) 2 0)) -> nil
    (print (SUB-LIST '(a b c d e f g) 2 3)) -> (c d e)
|#
(defun SUB-LIST (L START LEN)
    (cond 
        ((null L) nil)
        ((not (eql START 0)) (SUB-LIST (cdr L) (- START 1) LEN))
        ((not (eql LEN 0)) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
        (t ())
    )
)

(TERPRI)
(print (SUB-LIST '(a b c d) 0 3))
(print (SUB-LIST '(a b c d) 3 1))
(print (SUB-LIST '(a b c d) 2 0))
(print (SUB-LIST '(a b c d e f g) 2 3))

;-------------------------------------------------------------------------------

#|Q5.
If we get the length of the list, we get two scenarios: even length or odd length
Even Length: Split the list from 0 to len/2 and len/2 to len
Odd Length: Split the list from 0 to (len+1)/2 and (len+1)/2 to len.
Test Cases:
    (print (SPLIT-LIST '(a b c d))) -> ((a b) (c d))
    (print (SPLIT-LIST '(a b c d e))) -> ((a b c) (d e))
    (print (SPLIT-LIST '(a b c d e f))) -> ((a b c) (d e f))
|#

(defun SPLIT-LIST (L)
    (let ((x (length L)))
        (cond 
            ((evenp x) (list (SUB-LIST L 0 (/ x 2)) (SUB-LIST L (/ x 2) x)))
            ((oddp x) (list (SUB-LIST L 0 (/ (+ x 1) 2)) (SUB-LIST L (/ (+ x 1) 2) x)))
        )
    )
)

(TERPRI)
(print (SPLIT-LIST '(a b c d)))
(print (SPLIT-LIST '(a b c d e)))
(print (SPLIT-LIST '(a b c d e f)))

;-------------------------------------------------------------------------------

(defun BTREE-HEIGHT (TREE))
(defun LIST2BTREE (LEAVES))
(defun BTREE2LIST (TREE))
(defun IS-SAME (E1 E2))