;
; CS161 Spring 2020 HW6 Problem 3: Graph coloring to SAT conversion
;

; General util.
(defun reload()
    (load "hw6.lsp")
)

; returns the index of the variable that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
(defun node2var (n c k)
    (+ (* (- n 1) k) c)
)

; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
; Returns (7 8 9) for (at-least-one-color 3 1 3), which basically
; says atleast 1 needs to be true for the clause to be true.
(defun at-least-one-color (n c k)
    (cond 
        ((> c k) '())
        (t (append (list (node2var n c k)) 
                   (at-least-one-color n (+ c 1) k)
           )
        )
    )
)

; Helper for at-most-one-color to return sublists for current color
; and index of max color.
(defun at-most-one-color-helper (cur maximum)
    (cond 
        ((= cur maximum) '())
        (t (append (at-most-one-color-helper cur (- maximum 1))
                   (list (list (- cur) (- maximum))))
        )
    )
)

; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
(defun at-most-one-color (n c k)
    (cond 
        ((= c k) '())
        (t  (let ((current (node2var n c k))
                  (max_ (node2var n k k)))
                 (append (at-most-one-color-helper current max_)
                         (at-most-one-color n (+ c 1) k)
                 )
            )
        )
    )
)

; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
(defun generate-node-clauses (n k)
    (let ((atleast_one (at-least-one-color n 1 k))
          (atmost_one  (at-most-one-color n 1 k)))
         (append (list atleast_one) atmost_one)
    )
)

; Helper for generate-edge-clauses. Incrementally adds all pairs of variables
; that can not both be true at the same time. Eg. (-1 -4) (-2 -5) (-3 -6) indicates
; that for a graph with atleast 2 nodes and 3 total colors, node 1 and node 2 must 
; both not have the same color.
(defun generate-edge-clauses-helper (e1 e2 k)
    (cond 
        ((= k 0) '())
        (t (append 
              (list (list (- e1) (- e2)))
              (generate-edge-clauses-helper (+ e1 1) (+ e2 1) (- k 1)))
        )
    )
)

; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
(defun generate-edge-clauses (e k)
    (let ((current_edge1 (node2var (first e) 1 k))
          (current_edge2 (node2var (second e) 1 k)))
         (generate-edge-clauses-helper current_edge1 current_edge2 k)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun

;; (graph-coloring-to-sat "graph1.txt" "graph1_sol.txt" 4)
;; (graph-coloring-to-sat "graph2.txt" "graph2_sol.txt" 8)