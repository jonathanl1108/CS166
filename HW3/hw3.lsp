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
  (load "hw3.lsp")
  )

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
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

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
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

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

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; if goal state is reached then there should be no box left and the keeper is on goal 
;==================================================================
; This is a helper function for goal test, to check if a keeper star is being spot in the map
; Input -> current state
; output -> if found a keeper-star t elae nil 
; algothim -> traverse the map check check any match 
; testing :
; (goal-test '((1 1 1 1 1 1)
; 	   (1 1 0 0 1 1)
; 	   (1 0 0 0 0 1)
; 	   (1 4 5 5 4 1)
; 	   (1 0 0 0 0 1)
; 	   (1 1 3 1 1 1)  ; 3 keeper not goal
; 	   (1 1 1 1 1 1))) ; f

; (goal-test '((1 1 1 1 1 1)
; 	   (1 1 0 0 1 1)
; 	   (1 0 0 0 0 1)
; 	   (1 4 5 5 4 1)
; 	   (1 0 0 0 0 1) 
; 	   (1 1 6 1 1 1)  
; 	   (1 1 1 1 1 1))) ; t

(defun found-Keeper-star (s)
	(cond
		( (null s) nil)
		( (atom s) ( if( isKeeperStar s) t nil))
		( t ( or ( found-Keeper-star (car s))( found-Keeper-star (cdr s))))
	);end cond
);end fun 
; The game ends as soon as every movable entity, including every box and the keeper, is in a goal position
(defun goal-test (s)
	(let( (numBox (h1 s)) (is-keeper-on-star (found-Keeper-star s) )) ; my var lisy 
		( if ( and(= numBox 0 ) is-keeper-on-star)t
			nil ; else nil 
		);end if 
	)
 );end defun

;=======================================================================

; Write a function called get-square that takes in a State S,
;  a row number r, and a column number c.
;  It should return the integer content of state S at square (r,c). 
;  If the square is outside the scope of the problem, return the value of a wall.

; helper function for get square inspired by getKeeperPosition
; Input  -> the desire row(single list) and the c is the pos of the target
; Output -> the item at (c r)
( defun get-c-at-r(row c)
	(cond
		( (null row) nil )
		( (or ( < c 0) ( > c (- (length row) 1))) wall);If the square is outside the scope of the problem, return the value of a wall
		(t
			( if( = c 0)(first row)
				(get-c-at-r (cdr row) (- c 1));end if c not 0 
			);end if 
		);end default  
	);end cond
);end fun 

; Function to get the item at ( c r )
; Input
	; s-> current state
	; c-> the column of the target at row r
	; r-> the row target stays
; Output -> the item at (c r)
(defun get-square (s r c)
	( cond 
		( (null s) nil)
		( (or ( < r 0) ( > r (- (length s) 1))) wall);If the square is outside the scope of the problem, return the value of a wall
		(t
			( if( = r 0)
				(get-c-at-r (car s) c)
				(get-square (cdr s) (- r 1) c) ;else move to r 
			);end if 
		);end default 
	);end cond 
);end fun 



;=======================================================================
; Write a function called set-square that takes 
; in a state S, a row number r, a column number c, and a square content v (integer). 
; This function should return a new state S’ that is obtained by setting the square (r,c) to value v. 
; This function should not modify the input state.
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

; inspired by getKeeperPosition
; helper function for setsq2 inspired by getKeeperPosition
; Input  -> the desire row(single list) and the c is the pos of the target
; Output -> the updated list 
( defun set-sq-at-rc( row c v) ; return a list 
	( cond 
		( (null row) nil)
		( (or ( < c 0) ( > c (- (length row) 1))) row);If the square is outside the scope of the problem, return the value of a wall
		(t  ( let( ( curr (car row))( tl (cdr row)))
				( if( = c 0)
					( cons v tl)
					( cons curr (set-sq-at-rc tl (- c 1) v)) ;else move to r 
				);end if 
			);end let
		);end default 
	);end cond
);end fun  

; inspired by getKeeperPosition
; helper function for set-square inspired by getKeeperPosition
; Input  -> 
       ; s-> current state
       ; r->the desire row(single list)
       ; c-> is the pos of the target
       ; v->item to be replaced 
; algorithm cut the list until row is 0 and call set-sq-at-rc
; Output -> the updated list 
(defun setsq2 (s r c v)
	( cond 
		( (null s) nil)
		( (or ( < r 0) ( > r (- (length S) 1))) S);If the square is outside the scope of the problem, return the value of a wall
		(t
			(let( (curr-row (car S)) (tl (cdr S)))
				( if( = r 0); cons( list (list of list ))
					(cons (set-sq-at-rc curr-row c v) tl ) ; returns a list of list 
					(cons curr-row (setsq2 tl (- r 1) c v)) ;add row to(list of list)
				);end if 
			);end let
		);end default 
	);end cond
)
; inspired by getKeeperPosition
; function for set-square 
; Input  -> 
       ; s-> current state
       ; r->the desire row(single list)
       ; c-> is the pos of the target
       ; v->item to be replaced 
; algorithm call setsq2 to reverse r c with (x y)
; Output -> the updated list 
( defun set-square (S r c v)
	(setsq2 S c r v)
);end fun 

;=======================================================================

; Write a function try-move that takes in a state S and a move direction D. 
; This function should return the state that is the result of moving the keeper 
; in state S in direction D. NIL should be returned if the move is invalid 
; (e.g. there is a wall in that direction). How you represent a move direction is up to you. 
; Remember to update the content of every square to the right value. 
; Refer to Table 1 for the values of different types of square.
; ________________________
; Blank 		 | 0 |‘ ‘ 
; ---------------+---+----
; Wall  		 | 1 |‘#’
; ---------------+---+----
; Box   		 | 2 |‘$’
; ---------------+---+----
; keeper   	     | 3 |‘@’
; ---------------+---+----
; Goal 		     | 4 |‘.’
; ---------------+---+----
; Box + goal 	 | 5 |‘*’
; ---------------+---+----
; Keeper + goal  | 6 |‘+’
; ------------------------
; top 1 right 2 down 3 left 4 

; functions checkers to select the dir passed in by try-move
(defun isUp (v)
  (= v 1)
  )
(defun isRight (v)
  (= v 2)
  )
(defun isDown (v)
  (= v 3)
  )
(defun isLeft (v)
  (= v 4)
  )

; the function produced the future location from (x y) in the direction D
; this is a helper function to check if next move is valid indeed 
( defun simulate (x y d)
	( cond
		((isUp d)    (list x (- y 1)))
		((isRight d) (list (+ x 1) y))
		((isDown d)  (list x (+ y 1)))
		((isLeft d)  (list (- x 1) y))
		(t nil) ; shoudnt get here but oh well
	);end cond
);end if 

; This is a helper funnction for try-moves  that
; helps to upfate the state after the current item 
; is being moved to the new locaiton 
; the prv location will then be replaced by blank or star
; depends on the item type 
; Input->
 ;    org_x -> prev x coordinate
 ;    org_y -> prev y coordinate
 ;    org_s -> prev state
 ;    whoami -> the object on (org_x org_y)
; output-> un clean state where the prv (x y) is repalced with proper 
; Alg: 
    ; if the item is box or kepper set ( org-x org-y) with blank 
    ; if the item is boxstar or kepperstar set ( org-x org-y) with star

( defun updated-prv-State ( whoami org_s org_x org_y)
	(cond
		( (or(isKeeper whoami)(isBox whoami))(set-square org_s org_x org_y blank )) ;if keeper then replace the org xy with blank
		( (or(isKeeperStar whoami)(isBoxStar whoami))(set-square org_s org_x org_y star )); if is keeper star then replace the org xy with star 
		(t org_s) ;else just org s since the keeper is not here
	);end cond
);end fun 

;try-move is the helper function for next state 
;this function is desgined to check if make a move
; from x y in direction D in state S is posssible 
; input
; 	S-> current state
; 	D-> direction to move
; 	kx-> the init x coordinate
; 	ky-> the init y coordinate
; output
; 	nil if the move is bad
; 	else a update state 
; alg:
; 	check base case if any of input is null return nil
; 	- check if the direction is vlaid in range 
; 	- check if the item next is wall -> if so nil
; 	- else depned on the item
; 		update prv state 
; 		if keeper
; 			if next item is blank move
; 			if next is star move a change from 3 to 6 in new x y
; 			if next is box and valid to move it
; 				if the next is box star move and change 3 to 6 in new x y
; 				else just move and replace with 3
; 			else nil 
; 	    if box 
; 	    	if next is blank move
; 	    	if next is star move and replace with 5 not 2
; 	    	else nil

( defun try-move(S D kx ky)
	( cond
		( (or (null kx) (null ky)) nil) ;base case 
		( (or (< D 1) (> D 4)) nil);base case 
		; now lets try to move from x y in D
		(t
			(let*(
				( result-move (simulate kx ky D))
				( next_x (first  result-move))
				( next_y (second result-move))
				( next_sqr (get-square S next_y next_x))) ; end my var list 
			(if(isWall next_sqr)nil ;dont have to check anymore 
				(let*( 
					(whoIam (get-square S ky kx))
					(ready-to-move-State (updated-prv-State whoIam S kx ky))); know what item i am and update prv state 
				( if(or(isKeeper whoIam)(isKeeperStar whoIam))
					(cond ; we can hit blank, box, goal, boxstar
						((isBlank next_sqr) (set-square ready-to-move-State next_x next_y keeper))
						((isStar next_sqr) (set-square ready-to-move-State next_x next_y keeperstar))
						((or(isBox next_sqr)(isBoxStar next_sqr))
							( let( (next_state (try-move ready-to-move-State D next_x next_y) ))
								(if(null next_state)nil
									( try-move (set-square next_state kx ky whoIam) D kx ky	)
								);end if for go again 
							);end next state compute
						 );end box related case 
						(t nil)
					);end for keeper case
					(cond ; we can hit blank, box, goal, boxstar
						((isBlank next_sqr) (set-square ready-to-move-State next_x next_y box))
						((isStar next_sqr)  (set-square ready-to-move-State next_x next_y boxstar))
						; ((or(isBox next_sqr)(isBoxStar next_sqr))
						; 	( let( (next_state (try-move ready-to-move-State D next_x next_y) ))
						; 		(if(null next_state)nil
						; 			( try-move (set-square next_state kx ky whoIam) D kx ky	)
						; 		);end if for go again 
						; 	);end next state compute
						;  );end box rel
						(t nil)
					);end for box case
				);end if =>> btw keeper and box 
				);end let
			);end if 
		    );end let
		);end default 
	);end cond 
);end fun


;=======================================================================


; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
; next state will make a list ->list->list
; which is the 4 possible direction up down right left 
; and clear lsit will remove nil if that direction is impossible to move 
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 1 x y );up
	 			   (try-move s 2 x y );right
	 		       (try-move s 3 x y );down
	 		       (try-move s 4 x y );left
	 	))
	 )
    (cleanUpList result);end
   );end let
 );end fun 



; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; s is a list of list 
; scan through each row 
; and count the number of box in map
; ptherwise the map should have no box and retrun 0
(defun h1 (s)
	(cond
		( (null s) 0)
		( (atom s) ( if( isBox s) 1 0))
		( t (+ ( h1 (car s)) (h1 (cdr s))))
	);end cond
  );end fun

; (h1 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
; 	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
; 	    (1 1 1 1 1 0 2 0 0 0 0 1 1 1 1 1)
; 	    (0 0 0 0 0 1 0 0 0 0 1 0 0 2 0 0)
; 	    (0 0 0 0 0 0 1 0 0 1 0 2 0 0 0 0)
; 	    (0 0 0 2 0 0 0 0 3 0 0 0 0 0 0 0)
; 	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
; 	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
; 	    (1 1 1 1 1 0 2 0 0 0 0 1 1 1 1 1)
; 	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
; 	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
; 	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
; 	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
; 	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0))) ; 7
;=====================================================================

; helper function to find all the box cord 


; ( defun boxlist(s r c)
; 	( cond
; 		((null s) nil)
; 		((or(< r 0)(< c 0)) nil)
; 		((atom s)
; 			(if( isBox s) (list( list (- c 1) r))
; 				nil
; 			))
; 		(t ( if( > c 0)
; 			  (append (boxlist (first s) r c)(boxlist (cdr s) r (+ c 1))) ;check same rol
; 			  (append (boxlist (first s) r (+ c 1))(boxlist (cdr s) (+ r 1) c)) ; check curr row and rest 
; 			)
; 		);end default case 
; 	);end cond
; )
( defun INIF()
	(+ 1 4998)
)
;; this is a function that compaute the compute-manhattan-distance
;; base on the formual abs(x2-x1) + abs(y2-y1)
( defun compute-distance(cord1 cord2)
	(let( 
		(px (first cord1))(py (second cord1))
		(qx (first cord2))(qy (second cord2))
		);end my var list
	(cond
		((or (null cord1)(null cord2)) 0 )
		((or (< px 0)(< py 0)) 0 )
		((or (< qx 0)(< qy 0)) 0)
		(t
			(+ (abs (- qx px))(abs (- qy py)))
		)

	);end cond
	);end let
);end fun
( defun compute-manhattan-distance(cord1 cord2)
	(let( 
		(px (first cord1))(py (second cord1))
		(qx (first cord2))(qy (second cord2))
		);end my var list
	(cond
		((or (null cord1)(null cord2)) (INIF) )
		((or (< px 0)(< py 0)) (INIF) )
		((or (< qx 0)(< qy 0)) (INIF))
		(t
			(+ (abs (- qx px))(abs (- qy py)))
		)

	);end cond
	);end let
);end fun

; ( defun side-dead-lock (s b g)
; 	( or (side-dead-lock-x s b g)(side-dead-lock-y s b g))
; )

( defun side-dead-lock( s bpos gpos)
	( let*(
		( xpos (first bpos))
		( ypos (second bpos))
		( gx (first gpos))
		( gy (second gpos))
	) ;end var list
	(or ( and (or( isWAll (get-square s  ypos (- xpos 1) ))(isWAll (get-square s  ypos  (+ xpos 1) )))( not(= gx xpos)))
		( and (or( isWAll (get-square s (- ypos 1) xpos ))(isWAll (get-square s (+ ypos 1) xpos )))( not(= gy ypos)))
	)
  )
);end fun 
 
; this fucntion will be able to find the closet goal from a box
; alg :
	; the fucntion will generate a list of distenace btw goals and the current boxlist
	; by compute-manhattan-distance, then pick the min of it
	; if the min = INIF then we know the box is not acived able then jsut give 0 


; corner dead lock will elminae case where box is stuck in corner
( defun corner-dead-lock ( s bpos )
	( let*(
		( xpos (first bpos))
		( ypos (second bpos))
	) ;end var list
	(and (or( isWAll (get-square s (- ypos 1) xpos ))(isWAll (get-square s (+ ypos 1) xpos )))  
		(or( isWAll (get-square s  ypos (- xpos 1) ))(isWAll (get-square s  ypos  (+ xpos 1) ))) )
	);end let
);end fun 

( defun find-closest-goal( curr_box goalist)
	(cond
		( (null goalist)  (INIF) )
		( (null curr_box) (INIF) )
		(t
		   (let((m_min (min (compute-manhattan-distance curr_box (car goalist)) ( find-closest-goal curr_box (cdr goalist)) )))
		   	( if( = m_min (INIF))0
		   		m_min
		   	)
		   	);end let 
		)
	);end cond
);end fun 

; this function will be able to compute the heuristic value from the 
; bipart boxes and goals grapgh with the help of the find-closest-goal

( defun heur_manhattan_distance ( s boxlist goalist)
	(cond
		( (or (null boxlist) (null goalist)) 0)
		( (corner-dead-lock s (car boxlist)) (heur_manhattan_distance s (cdr boxlist) goalist))
		(t
			(+(find-closest-goal (car boxlist) goalist)(heur_manhattan_distance s (cdr boxlist) goalist))
		)
	);end cond
);end fun

; this will find the box that is closet to the keepers location 
( defun keeper-to-boxes ( keeperpos boxes)
	; (print "cord")
	; (print keeperpos )
	; (print boxes )
	( cond
		((null boxes) 0)
		(t (+ (compute-distance keeperpos (car boxes)) (keeper-to-boxes keeperpos (cdr boxes))))
	)
);end fun
( defun boxes-pos(s)
	(objlist s 0 0 box)
)
( defun goals-pos(s)
	(objlist s 0 0 star)
)
; a generaic funciton that can collect list of location of the requested item 
( defun objlist(s r c v)
	( cond
		((null s) nil)
		((or(< r 0)(< c 0)) nil)
		((atom s)
			(if(= s v) (list( list (- c 1) r))
				nil
			))
		(t ( if( > c 0)
			  (append (objlist (first s) r c v)(objlist (cdr s) r (+ c 1) v) ) ;check same rol
			  (append (objlist (first s) r (+ c 1) v)(objlist (cdr s) (+ r 1) c v)) ; check curr row and rest 
			)
		);end default case 
	);end cond
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;

(defun h105182103 (s)
	(+(keeper-to-boxes (getKeeperPosition s 0) (boxes-pos s)) (heur_manhattan_distance s (boxes-pos s) (goals-pos s)))
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

;(51)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(43)
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
