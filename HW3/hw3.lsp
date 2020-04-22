;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp"))

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star))

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank))

(defun isWall (v)
  (= v wall))

(defun isBox (v)
  (= v box))

(defun isKeeper (v)
  (= v keeper))

(defun isStar (v)
  (= v star))

(defun isBoxStar (v)
  (= v boxstar))

(defun isKeeperStar (v)
  (= v keeperstar))

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; Logic: For a game to be complete, there must be no more
; boxes or keepers who are not on a goal. Therefore, for 
; the game to end and be a goal state, there must be no 2's 
; (box) or 3's (keeper) on any of the rows in the state.
(defun goal-test (s)
	(cond 
		((null s) t)
		((and (equal (count 2 (car s)) 0)
			  (equal (count 3 (car s)) 0)) 
			  (goal-test (cdr s)))
		(t '())
	)
)

; Returns the value at (r,c) in state s. It does this by first removing 
; the first r rows from s, and then removing the first c columns from the
; the first row of what remains. Then we simply extract the head of the 
; remaining list.
; Want to check and return the value of a wall if:
;	1. rows/columns is less than 0
;	2. If (r,c) does not exist in s. nthcdr returns nil if it does not 
;      my (r,c) extraction does not exist.
(defun get-square (s r c)
	(cond
		; Left and top edges of the board
		((or (< r 0) (< c 0)) 1)
		((not (nthcdr c (first (nthcdr r s)))) 1)
		(t (first (nthcdr c (first (nthcdr r s)))))
	)
)

; Sets the value at (r,c) in state s to v. It does this by essentially 
; by creating a new row with value v, and then appending it to the first 
; r-1 rows. Then, we append the remaining rows to the first result.
(defun set-square (s r c v)
	(let ((row (first (nthcdr r s))))
		 (append 
			(butlast s (- (length s) r))
			(list (append 
					  (butlast row (- (length row) c))
					  (cons v '())
					  (nthcdr (+ c 1) row)
				  )
			)
			(nthcdr (+ r 1) s)
		 )
	)
)

; Returns a valid state if move is possible, otherwise returns nil
; The basic idea behind this function is that we check if move d can be performed from 
; state s. If its a blank we can move to that position, if it's a wall then we can't 
; move so we return nil. If it's a box, then we need to check the subsequent space to
; see if the box has the ability to move in the desired direction of movement. It can 
; move if the subsequent space is a blank or goal, but cannot move if its another box 
; or wall. 
(defun try-move (s d)
	(let* 
		   ((currentPos (getKeeperPosition s 0))
			(col (car currentPos))
			(row (cadr currentPos))
			(currLoc (cond 
					((= (get-square s row col) 3) 0)
					(t '4)))
			; single - one step
			(single_col (cond 
					((= d 0) (- col 1)) 
					((= d 1) (+ col 1)) 
					(t col)))
			(single_row (cond 
					((= d 2) (- row 1)) 
					((= d 3) (+ row 1)) 
					(t row)))
			; double - two steps
			(double_col (cond 
					((= d 0) (- col 2)) 
					((= d 1) (+ col 2)) 
					(t col)))
			(double_row (cond 
					((= d 2) (- row 2)) 
					((= d 3) (+ row 2)) 
					(t row)))
			(single_step_loc (get-square s single_row single_col))
			(double_step_loc (get-square s double_row double_col))
		)
		(cond
			; Set new position to 3 to indicate keeper movement, and set current pos to 0 to indicate blank
			((isBlank single_step_loc) (set-square (set-square s single_row single_col keeper) row col currLoc))
			; Next step is a wall, so we return nil since we can't move
			((isWall single_step_loc) nil)
			; If the next step is a box, then we must address 2 cases: Is the empty space following the box
			; in the same direction a blank, goal, or wall?
			((isBox single_step_loc)
				(cond
					;If blank, move box to this location and keeper to the box's location.
					((isBlank double_step_loc) (set-square (set-square (set-square s single_row single_col keeper) row col currLoc) double_row double_col box)) 
					;If goal, repeat change of numberings accordingly.
					((isStar double_step_loc) (set-square (set-square (set-square s single_row single_col keeper) row col currLoc) double_row double_col boxstar))
					; Likely to be another wall or box, so we just return nil
					(t nil)
				))
			; Set new position to indicate keeper on goal state
			((isStar single_step_loc) (set-square (set-square s single_row single_col keeperstar) row col currLoc))
			(t 
				(cond
					((isBlank double_step_loc) (set-square (set-square (set-square s single_row single_col keeperstar) row col currLoc) double_row double_col box)) 
					((isStar double_step_loc) (set-square (set-square (set-square s single_row single_col keeperstar) row col currLoc) double_row double_col boxstar))
					(t nil)
				)
			)
		)
	)
)

; This is the successor function. Returns all valid states from the current state.
; Up, down, left and right are represented by 0, 1, 2 and 3 respectively.
(defun next-states (s)
	(cleanUpList (list (try-move s 0) (try-move s 1) (try-move s 2) (try-move s 3)))
)

; Logic: Return 0 as trivial heuristic.
; f(n) = g(n) + h(n) ==> f(n) = g(n) so this is admissable.
(defun h0 (s)
  '0
)

; This heuristic calculates the number of misplaced boxes for a given state
; Logic: Recursively count the number of boxes (represented by 2) in each row
;        and return the sum.
; Is this an admissable heuristic? Yes
; - The number of misplaced boxes is ATLEAST equal to the number of steps 
;	we would need to move all the boxes on top of a goal. The actual cost is 
;	equal to this in the best case, or would be even greater. Therefore, 
;	this heuristic does not overestimate the actual cost of reaching the goal state.
(defun h1 (s)
	(cond
		((null s) 0)
		((+ (count 2 (car s)) (h1 (cdr s))))
	)
)

; Get coordinates/position of all goals. Recursively visits all elements, and 
; if it is a star (goal), then it adds the coordinates in (c, r) manner to the list
(defun getGoalCoords (s r c)
	(cond 
		((null s) nil)
		((atom s) 
			(cond
				((isStar s) (list (list (- c 1) r)))
				(t 'nil)
			)
		)
		(t 
			(cond
				((= c 0) (append (getGoalCoords (car s) r (+ c 1)) (getGoalCoords (cdr s) (+ r 1) c)))
				(t (append (getGoalCoords (car s) r c) (getGoalCoords (cdr s) r (+ c 1))))
			)
		)
	)
)

; Get coordinates/position of all goals. Recursively visits all elements, and 
; if it is a box, it adds the coordinates in (c, r) manner to the list.
(defun getBoxCoords (s r c)
	(cond 
		((null s) nil)
		((atom s) 
			(cond
				((isBox s) (list (list (- c 1) r)))
				(t 'nil)
			)
		)
		(t 
			(cond
				((= c 0) (append (getBoxCoords (car s) r (+ c 1)) (getBoxCoords (cdr s) (+ r 1) c)))
				(t (append (getBoxCoords (car s) r c) (getBoxCoords (cdr s) r (+ c 1))))
			)
		)
	)
)

; Get coordinates for keeper and boxes
(defun getBoxAndKeeperCoords (s r c)
	(append (list (getKeeperPosition s 0)) (getBoxCoords s 0 0))
)

; Helper method to calculate absolute value
(defun absVal (x)
	(cond 
		((>= x 0) x)
		(t (- x))
	)
)

; Function to calculate manhattan distance, which is | x2 - x1 | + | y2 - y1 |
(defun manhattanDist (coords)
	(+ (absVal (- (first coords) (third coords))) (absVal (- (second coords) (fourth coords))))
)

; Calculates distance from box to each goal and return the minimum manhattandistance
(defun distanceBoxToGoals (box goals)
    (cond 
		((= (length goals) 1) (manhattanDist (append box (append (first goals) (second goals)))))
		((<= (manhattanDist (append box (first goals))) (distanceBoxToGoals box (rest goals))) 
				(manhattanDist (append box (first goals))))
		(t (distanceBoxToGoals box (rest goals)))
    )
)

; Add distances from each box and the keeper to the closest goal.
(defun addDistances (boxes goals)
    (cond 
		((= (length boxes) 0) 0)
        ((= (length boxes) 1) (distanceBoxToGoals (append (car boxes) (cdr boxes)) goals))
        (t (+ (distanceBoxToGoals (car boxes) goals) (addDistances (cdr boxes) goals)))
    )
)

; Custom heuristic function: We find the sum of the minimum manhattan distances between
; all keeper/boxes and the nearest goal to it. The reason I chose manhattan distance over
; euclidean distance is because it is always >= euclidean distance, thereby generating 
; a much larger h(n). However, in the best case, the actual distance >= h(n), so this 
; does not overestimate and it is an admissable heuristic.
(defun h304911796 (s)
	(cond 
		((null s) 0)
		(t (let ((boxes (getBoxAndKeeperCoords s 0 0))
			     (goals (getGoalCoords s 0 0)))
			(cond
				((or (null boxes) (null goals)) 0)
				(t (addDistances boxes goals))
			)
		))
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun