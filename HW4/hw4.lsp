;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

; This function takes a sign (which is either a -1 or 1) and the current 
; list of assigned variables, and adds a new variable assignment (the 
; next variable that has not been assigned) based on the selected sign.
; -1 represents false, and 1 represents true
; E.g. (assignVariable -1 (23 -24 25)) -> (-22 23 24 25)
(defun assignVariable (n sign assignments)
    (let ((next_num (- n (length assignments))))
          (cond 
              ((not sign) (append (list (- next_num)) assignments))
              (t (append (list next_num) assignments))
          )
    )
)

(defun checkVariable (var assignments)
    (cond 
        ((= (length assignments) 0) t)
        ((= var (- (car assignments))) nil)
        ((= var (car assignments)) t)
        (t (checkVariable var (cdr assignments)))
    )
)

(defun checkValidClause (clause assignments)
    (cond 
        ((= (length clause) 0) nil)
        ; Atleast 1 should be true for entire clause to be true
        (t (or (checkVariable (first clause) assignments)
               (checkValidClause (rest clause) assignments)))
    )
)

(defun checkCurrentAssignments (delta assignments)
    (if (not (= (length delta) 0)) 
        (and (checkValidClause (car delta) assignments)
              (checkCurrentAssignments (cdr delta) assignments)
        ) ;t
        t ;f
    )
)

; This function uses a greedy depth-first search to find a valid solution to
; the 3-sat problem.
(defun backtrack (n delta assignments)
    ; Check if current assignment is still valid. If not, try a different value
    ; for the same variable. Don't recurse further down this branch.
    (if (checkCurrentAssignments delta assignments)
        (cond
            ((equal (length assignments) n) assignments)
            (t  (let ((positiveAssignment (assignVariable n t assignments))
                      (negativeAssignment (assignVariable n nil assignments)))
                    (or (backtrack n delta positiveAssignment)
                        (backtrack n delta negativeAssignment))
                )
            )
        )    ;t
        'nil ;f
    )
)

; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
    (cond 
        ((or (= n 0) (equal (length delta) 0)) nil)
        (t (backtrack n delta '()))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You don't need to modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

(time (print (solve-cnf "./test.cnf")))
(time (print (solve-cnf "./cnfs/f1/sat_f1.cnf")))
;; (time (print (solve-cnf "./cnfs/f2/sat_f2.cnf")))
;; (time (print (solve-cnf "./cnfs/f3/sat_f3.cnf")))
;; (time (print (solve-cnf "./cnfs/f4/sat_f4.cnf")))
;; (time (print (solve-cnf "./cnfs/f5/sat_f5.cnf")))