; 


;; BFS
;Write a single pure LISP function, called BFS, 
; that performs a breadth-first search of a tree. 
; The func- tion should take a single argument that is the list representation of the tree, 
; and returns a single, top-level list of the terminal nodes in the order they would be visited 
; by a left-to-right breadth-first search. For ex- ample, (bfs '((A (B)) C (D))) would return (C A D B). 
; Do not use any auxiliary functions.
; =====================================================
; BFS 
; -----------------------------------------------------
; Input =>
; 	TREE: leaf node of the tree in list form
; Output =>
; 	List :top-level list of the terminal nodes in 
; 	      the order they would be visitet in bfs
; 	Nil : if tree is empty
; -----------------------------------------------------
; Algorithm
; -check the base case is the tree is null return nil
; -if the tree is just a single element return it
; - Actual BFS manchisism:
; 	- since we can't use loop or structure like queue 
; 	  therefore we have to think of another way, since 
; 	  we observed that the top level leaf node is the atom
; 	  within in the current list, therefore we exmaine the list
; 	  by visting each element:
; 	  	if it is atom ->add it to list and append with future atom(recusion on rest)
; 	  	else 
; 	  		move the current element to the back of the list (modify the lsit order)
; 	  		and do bfs on this modified list 
;------------------------------------------------------
; Test
; (bfs '((A (B)) C (D))) would return (C A D B)
; =====================================================

( defun bfs (TREE)
	( cond
		( (null TREE) NIL ); base case
		( (atom TREE)  TREE); base case 
		( (listp TREE )
			( let( ( hd (car TREE)) (tl (cdr TREE)))
				( cond
					( (atom hd ) ( append (list hd) (bfs tl) )) ;check if first list only have  one element 
					( t ( bfs ( append tl hd ) )); if hd is a list queue it 
				)
			) ; end let env 
		);end list case 
	); end cond 
); end fun

; Write a single pure LISP function, called DFS, 
; that performs a depth-first search of a tree. 
; The function should take a single argument that is 
; the list representation of the tree, and returns a single,
;  top-level list of the terminal nodes in the order they would 
;  be visited by a right-to-left depth-first search. 
;  For example, (dfs '((A (B)) C (D))) would return (D C B A)


; =====================================================
; DFS 
; -----------------------------------------------------
; Input =>
; 	TREE: leaf node of the tree in list form
; Output =>
; 	List :top-level list of the terminal nodes in 
; 	      the order they would be visitet in dfs
; 	Nil : if tree is empty
;-------------------------------------------------------
; var name
; hd : head of list 
; tl : rest oflist
; -----------------------------------------------------
; Algorithm
; -check the base case is the tree is null return nil
; -if the tree is just a single element return it as a list 
; - Actual DFS manchisism:
; 	- since we are doing right-to-left depth-first search.
; 	  	- travsere the list until we see the last element 
;		; 	- then call dfs on itself inorder to get the list form
;		; - else jsut append the reuslt of dfs on tl and dfs on hd
;------------------------------------------------------
;Test
;
;(dfs '((a(b))c (d(f(g h)))))       (H G F D C B A)
;(dfs '(A (B C) (D) (E (F G))))     (G F E D C B A)
; =====================================================


( defun dfs (TREE)
	( cond 
		( ( null TREE) NIL); base case 
		( ( atom TREE) (list TREE)) ; atom base case
		( t
			(let (( hd (car TREE)) ( tl (cdr TREE)) )
				(cond
					((not( null tl))( append (dfs tl)(dfs hd)));
					(t(dfs hd))
				); end list cond
			);end cond let var 
		);end list case 
	);end cond
); end fun



; =====================================================
; getDfsTopLevelAT
; -----------------------------------------------------
; Input =>
; 	TREE: leaf node of the tree in list form
;	Leval: the depth that determien the look of the current tree 
; Output =>
; 	List :top-level list of the terminal nodes in 
; 	      the order they would be visitet in dfs
; 	Nil : if tree is empty or the level is already 0
;-------------------------------------------------------
; var name
; hd : head of list 
; tl : rest oflist
; -----------------------------------------------------
; Algorithm
; -check the base case is the tree is null return nil
; -check the base case is the level is 0 return nil
; -if the tree is just a single element return it as a list 
; -if the level is jsut one, just find all the atom in the current list 
;  and return them as list in order of vist 
; - if the level is greater the 1
; - examin the list in order sine we are doing left-right dfs
  	; - for current hd get the top level of the on level-1, while the rest keep serch on current level
  	;   by recusion 
  	; - finally append the result in to a list  
;------------------------------------------------------
;Test
; ( getDfsTopLevelAT '((A (B)) C (D)) 1)
; (dfid '((A (B)) C (D)) 0)
;     => NIL
; (dfid '((A (B)) C (D)) 1)
;     => (C)
; (dfid '((A (B)) C (D)) 2)
;     => (C A C D)
; (dfid '((A (B)) C (D)) 3)
;     => (C A C D A B C D)
; =====================================================

( defun getDfsTopLevelAT(TREE Level)
	( cond
		( (or (null TREE ) (= 0 Level )) NIL) ; base case 
		( (atom TREE) (list TREE)) ; base case if atom just listed it 
		( (= 1 Level) ; simple case one level => return any atom in the current list 
			( let( (hd (car TREE))(tl (cdr TREE)))
				(cond
					( (atom hd) (append (list hd)( getDfsTopLevelAT tl Level)))
					( t (getDfsTopLevelAT tl Level))
				);end cond
			)
		);end cond le =1 
		(t ; if level >1 recur from left to right 
			( let ( (nextLev (- Level 1)) (hd (car TREE))(tl (cdr TREE)))
				(append  (getDfsTopLevelAT hd nextLev ) ( getDfsTopLevelAT tl Level ))
			)
		); end default case 

	);end cond 

);end fun

; =====================================================
; DFID
; -----------------------------------------------------
; Input =>
; 	TREE: leaf node of the tree in list form
;	MAX_DEPTH: the depth that determien the look of the current tree 
; Output =>
; 	List :top-level list of the terminal nodes in 
; 	      the order they would be visitet in dfs
; 	Nil : if tree is empty or the level is already 0
; -----------------------------------------------------
; Algorithm
; -check the base case is the tree is null return nil
; -check the base case is the level is 0 return nil
; -check the base case is the level is 1 just print the atom at the current list 
; - other case just append the result of getDfsTopLevelAT 0 1 2 3 .... MAX_DEPTH
;------------------------------------------------------
;Test
; ( getDfsTopLevelAT '((A (B)) C (D)) 1)
; (dfid '((A (B)) C (D)) 0)
;     => NIL
; (dfid '((A (B)) C (D)) 1)
;     => (C)
; (dfid '((A (B)) C (D)) 2)
;     => (C A C D)
; (dfid '((A (B)) C (D)) 3)
;     => (C A C D A B C D)
; [13]> (dfid '(A (B C) (D) (E (F G))) 0)
; NIL
; [14]> (dfid '(A (B C) (D) (E (F G))) 1)
; (A)
; [15]> (dfid '(A (B C) (D) (E (F G))) 2)
; (A A B C D E)
; [16]> (dfid '(A (B C) (D) (E (F G))) 3)
; (A A B C D E A B C D E F G)

; =====================================================

( defun DFID ( TREE MAX_DEPTH )
	( cond
		( (or(= MAX_DEPTH 0) (null TREE)) NIL)
		( (= 1 MAX_DEPTH)(getDfsTopLevelAT TREE MAX_DEPTH ))
		(t (append (DFID  TREE (- MAX_DEPTH 1) )(getDfsTopLevelAT TREE MAX_DEPTH )) )
	); end cond
); end fun 




; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
; =====================================================
; final-state
; -----------------------------------------------------
; Input =>
;	state => a current state
; Output =>
; 	t : if it is (3 3 NIL)
; 	Nil : otherwise 
; -----------------------------------------------------
; =====================================================
(defun final-state (s)
	( equal '(3 3 NIL) s) 
);end fun 

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

; =====================================================
; next-state 
; -----------------------------------------------------
; Input =>
; 	s:  a current state 
;	m: number of missionaries to move from the current side
;   c: number of cannibals to move from the current side
; Output =>
; 	List : if m c is valid operation then apply it and return the reult state
; 	Nil  : if m c operation is invlaid 
; -----------------------------------------------------
; Algorithm
; -check if m c is valid 
	; - move more c and m then we have 
	; - There must be at least one person in the boat to cross the river
	; - boat can only carry two
	; - if m c pass the basic test will this operation reuslt in invalid state
	; 	-  more cannibals than missionaries on either side of the river
	; - if no problem apply the operation m c on s and return the new state 
;------------------------------------------------------
(defun next-state (s m c)
	(let( ( mcount (first s)) (ccount (second s)))
		(cond
			((or (> m mcount)(> c ccount)) nil); move more c and m then we have 
			((< (+ m c) 1)  nil ); There must be at least one person in the boat to cross the river
			((> (+ m c) 2)  nil ); boat can only carry two 
			; check this side 
			(t 
				(let*( 
						(ts (list (- mcount m)(- ccount c))) 
						(os (list (+ m (- 3 mcount )) (+ c (- 3 ccount ))))
						(tm (first ts))(tc (second ts))
						(om (first os))(oc (second os))		
					);======>>end my const var
				(cond
					( (and( > oc om)(> om 0)) nil)
					( (and( > tc tm)(> tm 0)) nil)
					(t ( list (append (list om)(list oc)(list (not(third s))))))
					)	
				);end let 
			);end default case 
		);end cond
	);end let
);end fun 



; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

; =====================================================
; succ-fn
; -----------------------------------------------------
; Input =>
; 	s:  a current state 
; Output =>
; 	List : list of list that are the avalble steps 
; -----------------------------------------------------
; Algorithm
; - use next state to genreate all possible states 
  ; m | c
  ; --+--
  ; 0 | 1
  ; --+--
  ; 0 | 2
  ; --+--
  ; 1 | 0
  ; --+--
  ; 1 | 1
  ; --+--
  ; 2 | 0
;------------------------------------------------------
(defun succ-fn (s)
	(append 
		(next-state s 0 1 )
		(next-state s 0 2 )
		(next-state s 1 0 )
		(next-state s 1 1 )
		(next-state s 2 0 )
	);end append 
); end fun

; =====================================================
; on-path
; -----------------------------------------------------
; Input =>
; 	s:  a current state 
;   STATES: ack of states visited by this depth-first search
; Output =>
; 	T NIL 
; -----------------------------------------------------
; Algorithm
; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
;------------------------------------------------------
(defun on-path (s states)
	(cond
		((null states) nil)
		( (equal s (car states)) t)
		( t ( on-path s (cdr states)))
	);end cond 
); end fun 

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)


; =====================================================
; mult-dfs
; -----------------------------------------------------
; Input =>
; 	states:  legal successor states from the current state 
;   path :   a stack of states from the initial state to the current state 
; Output =>
; 	List : list of STEPS from initial to final states 
; -----------------------------------------------------
; Algorithm
; check base case if the startes is null -> nil
; check if a path can be found from any of the successor states 
; recurecvly with the main fun mc-dfs 
;------------------------------------------------------
(defun mult-dfs (states path)
	(cond
		( (null states) nil)
		(t ( let( (findPath (mc-dfs (car states) path)))
			(cond 
				(findPath findPath)
				(t (mult-dfs (cdr states) path))
			)
			);end let
		);end default 
	);end cond 
);end fun 

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
; path should be ordered as: (S_n ... S_2 S_1 S_0)

; =====================================================
; mult-dfs
; -----------------------------------------------------
; Input =>
; 	s:  a current/initial state 
;   path :   a stack of states that recorded from the initial state to the current state / nil if initial  
; Output =>
; 	List : list of STEPS from initial to final states 
; -----------------------------------------------------
; Algorithm
; - check if the state s is final state -> jsut append the state to the path 
; - check if the state s is in a loop to aviod -> nil 
; - input the successer steps genreated by s and the appended list to the state 
; to the path into the helper function mult-dfs
;------------------------------------------------------

(defun mc-dfs (s path)
	(cond
		((final-state s) (append (list s) path));hecking if S is already the goal state,
		((on-path s path) nil);depth-first search does not revisit a node 
		(t (mult-dfs (succ-fn s )( append (list s) path)));depth-first search does not revisit a node 
	)
)



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))






