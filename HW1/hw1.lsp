
;Tree is ordered,so don't need to check beyond val N in ordered tree
(defun TREE-CONTAINS (N TREE)
    (cond 
        ;If empty list, return nil
        ((null (car TREE)) nil)
        ;If atom, then compare N to it directly, otherwise recurse on cdr
        ((atom (car TREE)) t)
        ;; ((cond ((eql (car TREE) N) t)
        ;;           (t (TREE-CONTAINS (cdr TREE))))))
        (t ())
    )
)

(defun TREE-MIN (TREE))
(defun TREE-ORDER (TREE))
(defun SUB-LIST (L START LEN))
(defun SPLIT-LIST (L))
(defun BTREE-HEIGHT (TREE))
(defun LIST2BTREE (LEAVES))
(defun BTREE2LIST (TREE))
(defun IS-SAME (E1 E2))

(print (TREE-CONTAINS 3 '(7 (1 2 3) 7 8)))
(print (TREE-CONTAINS 4 '((1 2 3) 7 8)))
;; (print (TREE-MIN'((123)78)))
;; (TREE-ORDER 3)
;; (TREE-ORDER '((1 2 3) 7 8))