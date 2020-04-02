
; Process atoms and lists separately. If head is an atom, check directly and recurse on 
; rest of the list. If head is a list itself, check that list and the remainder of the 
; parent list using an OR to parallely do both cases.
(defun TREE-CONTAINS (N TREE)
    (cond 
        ;If empty list, return nil
        ((null (car TREE)) nil)
        ;If atom, then compare N to it directly, otherwise recurse on remainder
        ((atom (car TREE)) 
            (cond
                ((eql (car TREE) N) t)
                (t (TREE-CONTAINS N (cdr TREE)))
            )
        )
        ;If first element is a list, check head of list
        ((listp (car TREE)) 
            (cond
                ((eql (caar TREE) N) t)
                ; Check in remaining elements in inner list, or cdr of parent list
                ((OR (TREE-CONTAINS N (cdr TREE)) (TREE-CONTAINS N (cdar TREE))) t)
            )
        )
        (t 'nil)
    )
)

(print (TREE-CONTAINS 3 '(0 (1 2 3) 7 8)))
(print (TREE-CONTAINS 4 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 5 '((1 2 3) 7 8)))
(print (TREE-CONTAINS 6 '((1 2 3) (6) 7 8)))

(defun TREE-MIN (TREE))





(defun TREE-ORDER (TREE))
(defun SUB-LIST (L START LEN))
(defun SPLIT-LIST (L))
(defun BTREE-HEIGHT (TREE))
(defun LIST2BTREE (LEAVES))
(defun BTREE2LIST (TREE))
(defun IS-SAME (E1 E2))