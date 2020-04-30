;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;

; This function takes a boolean (either t or nil) and the current 
; list of assigned variables, and adds a new variable assignment (the 
; next variable that has not been assigned) based on the selected sign.
; This adds values for variables in the reverse order, that is, from n to 1.
; E.g. (assignVariable nil (23 -24 25)) -> (-22 23 24 25)
; E.g. (assignVariable t (23 -24 25)) -> (22 23 -24 25)
(defun assignVariable (n sign assignments)
    (let ((next_num (- n (length assignments))))
          (cond 
              ((not sign) (append (list (- next_num)) assignments))
              (t (append (list next_num) assignments))
          )
    )
)

; This helper method takes in a variable from a clause, and the current
; assignment list and checks to see if the variable is valid/satisfiable.
; There are 3 cases:
;   1. The variable exists in assignments, in which case return true.
;   2. The negation of the variable exists in assignments, in which 
;        case we want to return false since we want the opposite.
;   3. The variable has not been assigned yet, in which case we will 
;        assume that it is true since we don't know its assignment yet
;        but can't negate the clause because of it.
(defun checkVariable (var assignments)
    (cond
        ((= (length assignments) 0) t)
        ((= var (- (car assignments))) nil)
        ((= var (car assignments)) t)
        (t (checkVariable var (cdr assignments)))
    )
)

; This helper method takes a given clause from delta, and checks whether 
; one of the variables in the clause can be satisfied based on the 
; current assignment list. If yes, then the entire clause is satisfied
(defun checkValidClause (clause assignments)
    (cond 
        ((= (length clause) 0) nil)
        ; Atleast 1 should be true for entire clause to be true 
        ; since all the variables have an OR'd together
        (t (or (checkVariable (first clause) assignments)
               (checkValidClause (rest clause) assignments)))
    )
)

; This helper method takes in delta and a list of variables that have been 
; assigned so far. It recursively checks if if each clause in the given 
; cnf/delta is satisfiable for the given assignment list.
(defun checkCurrentAssignments (delta assignments)
    (if (not (= (length delta) 0)) 
        (and (checkValidClause (car delta) assignments)
              (checkCurrentAssignments (cdr delta) assignments)
        ) ;t
        t ;f
    )
)

; This function uses a greedy depth-first search to find a satisfiable solution 
; to the 3-sat problem using backtracking. It takes 3 arguments, n, delta, and 
; the current assigned variables. It checks if the current list of assigned
; variables satisfy all the constraints, and if yes, it tries to assign the 
; next variable. It does this recursively until all variables are assigned and
; there is a valid variable assignment that represents a model of delta. It 
; is greedy, therefore it returns the first such solution it finds.
(defun backtrack (n delta assignments)
    ; Check if current assignment is still valid. If not, try a different value
    ; for the same variable. Don't recurse further down this branch.
    (if (checkCurrentAssignments delta assignments)
        (cond
            ((equal (length assignments) n) assignments)
            ; Try both positive and negative assignments. Will always pick a 
            ; positive assignment over a negative assignment if it is valid
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

; Main SAT driver method
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
; returns: list of n integers representing a model of delta
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

;; (time (print (solve-cnf "./test.cnf")))
;; (time (print (solve-cnf "./cnfs/f1/sat_f1.cnf")))
;; (time (print (solve-cnf "./cnfs/f2/sat_f2.cnf")))
;; (time (print (solve-cnf "./cnfs/f3/sat_f3.cnf")))
;; (time (print (solve-cnf "./cnfs/f4/sat_f4.cnf")))
;; (time (print (solve-cnf "./cnfs/f5/sat_f5.cnf")))