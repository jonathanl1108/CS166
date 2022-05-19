;1 Write a single Boolean LISP function, 
;called TREE-CONTAINS, which takes two arguments N and TREE, and checks whether number N appears in the ordered tree TREE
;#
;(TREE-CONTAINS 3 '((1 2 3) 7 8)) returns T
;(TREE-CONTAINS 4 '((1 2 3) 7 8)) returns NIL

;; TREE-CONTAINS 
;; INPUT :: 
	; N -> Target Number to be check in the list
	; TREE -> List of the order tree in format ( L m R)
;; OUTPUT ::
	; return true if N is found in TREE otherwisw return NIL	
;; Algorithm ::
	; Base Case if the Tree is null return NIL 
	; if the TREE contain only a single number return if N equals to that number
	; if the Tree contain one list check recurscvily with N
	; if N is M retrun true 
	; if N < M then check the L list recurscvily with N
	; else (N > M ) check the R list recurscvily with N


( defun TREE-CONTAINS (N TREE)
	(cond ( (null TREE ) NIL )
		  ( (numberp TREE)(= N TREE))
		  ( (null (cdr TREE)) (TREE-CONTAINS N (car TREE)) )
		  ( (= N (cadr TREE)) t)
		  ( (< N (cadr TREE))(TREE-CONTAINS N (car TREE)))
		  ( t (TREE-CONTAINS N (cddr TREE)))
	);end cond 
); end fun 

;2 Write a single LISP function, called TREE-MIN, which takes one argument TREE, and returns the minimum number appearing in the ordered tree TREE
;For example,
;(TREE-MIN '((1 2 3) 7 8)) returns 1

;; TREE-MIN 
;; INPUT :: 
	; TREE -> List of the order tree in format ( L m R)
;; OUTPUT ::
	; return min number is the tree is not empty 	
;; Algorithm ::
	; Base Case if the Tree is null return NIL 
	; if the TREE contain only a single number return that number
	; recursivlly do TREE-MIN with car Tree until only the first element is left 


( defun TREE-MIN (TREE)
	(cond ( (null TREE ) NIL )
		  ( (numberp TREE) TREE)
		  ( t (TREE-MIN (car TREE)))
	);end cond
); end fun

; 3 Write a single LISP function, called TREE-ORDER, which takes one argument TREE, and returns an pre-ordered list of the numbers appearing in the ordered tree TREE
; For example,
; (TREE-ORDER 3) returns (3)
; (TREE-ORDER '((1 2 3) 7 8)) returns (7 2 1 3 8)

;; TREE-ORDER 
;; INPUT :: 
	; TREE -> List of the order tree in format ( L m R)
;; OUTPUT ::
	; return list in the order of pre-order of the oreder list TREE
;; Algorithm ::
	; Base Case if the Tree is null return NIL 
	; if the TREE contain only a single number return that number in list 
	; append m with the results of left Subtree and right Subtree with TREE-ORDER recursivlly 

( defun TREE-ORDER (TREE)
    (cond ((NULL TREE) NIL)
          ((numberp TREE) (CONS TREE NIL))
          (t ( append (TREE-ORDER(cadr TREE)) (TREE-ORDER(car TREE)) (TREE-ORDER(cddr TREE))))
	);end cond
);end fun

; 4 Write a single LISP function, called SUB-LIST, that takes a list L and two non-negative integers START and LEN, and returns the sub-list of L starting at position START and having length LEN Assume that the first element of L has position 0
; For example,
; (SUB-LIST '(a b c d) 0 3) returns (a b c) (SUB-LIST '(a b c d) 3 1) returns (d)
; (SUB-LIST '(a b c d) 2 0) return s NIL

;; SUB-LIST(L START LEN)
;; INPUT :: 
	; L -> List of element
	; START -> a positive integer of the starting index
	; LEN -> size of the sublist from START
;; OUTPUT ::
	; sublist with size LEN with the element starting from START in original list L
;; Algorithm ::
	; Base Case if the Tree is null return NIL 
	; Valid input check on START and LEN 
	; Base Case if the LEN is 0 simply return NIL
	; Check validilty if START is out of bound 
	; Check validilty if the sublist from Start with size Len is in bound 
	; if the Start is 0 then start concationation of the first element then recursivlly called SUB-LIST while LEN get decrement by 1 
	; else we want to cut our list L with cdr until the first element is the desired starting point 


(defun SUB-LIST(L START LEN)
	(cond ((null L)  NIL )
  	    ((or ( < Len 0) (< START 0)) NIL)
  		((= LEN 0) NIL )
        ((> START (- (length L) 1)) NIL )
        ((> (+ LEN START) (length L)) NIL )
        ((= 0 START) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
        (t (SUB-LIST (cdr L) (- START 1) LEN))
	);end cond
);end fun

; 5 Write a single LISP function, called SPLIT-LIST, that takes a list L, and returns a list of two lists L1 and L2, in that order, such that
; - L is the result of appending L1 and L2;
; - Length of L1 minus length of L2 is 0 or 1
; For example,
; (SPLIT-LIST '(a b c d)) returns ((a b) (c d)) (SPLIT-LIST '(a b c d e)) returns ((a b c) (d e)) NOTE: ((a b) (c d e)) is incorrect; (SPLIT-LIST '(a b c d e f)) returns ((a b c) (d e f))
; You can call the function SUB-LIST from SPLIT-LIST

;; SPLIT-LIST(L)
;; INPUT :: 
	; L -> List of element
;; OUTPUT ::
	; returns a list of two lists L1 and L2, in that order, such that
	; - L is the result of appending L1 and L2;
	; - Length of L1 minus length of L2 is 0 or 1

;; Algorithm ::
	; Base Case if the Tree is null return NIL 
	; if the size of the list is even 
		;then call helper function SUB-LIST
			; First Half  -> SUB-LIST on L from 0 with the size of (half length of L - 1) [0 ,(len/2-1)]
			; Second Half -> SUB-LIST on L from (half of length ) with the size of (half length of L) [(len/2) ,(len/2+len/2)-1]
	; if the size of the list is odd 
		;then call helper function SUB-LIST 
			; since we want to have right centric and odd number devision actully gives decimal result, thus, 
			; First Half  -> SUB-LIST on L from 0 with the size of half of (length of L+1) [0 ,(len+1/2-1)]
			; Second Half -> SUB-LIST on L from ((half of length-1)+1) with the size of (half (length of L-1)) [(len+1)/2 ,(len+1)/2+(len+1)/2)-1]


; devide by 2 actually gives dec 
; (defun SPLIT-LIST(L)
;   	(cond ( (null L)  NIL )
;   	    ( (evenp (length L))  (list (SUB-LIST L 0 (/ (length L)  2)) (SUB-LIST L (/ (length L) 2) (/ (length L) 2)) ))
;   	    ( t ( list (SUB-LIST L 0 (/ (length L) 2)) ( SUB-LIST L (+ 1 (/ (length L) 2)) (/ (length L) 2) )))
; 	);end cond
; );end fun

; updated

(defun SPLIT-LIST(L)
  (cond ( (null L)  NIL )
  	    ( (evenp (length L))
  	    		( let*( ( size (length L)) (half (/ size 2)))
  	    		       (list (SUB-LIST L 0 half) (SUB-LIST L half half) )
  	    	    ); end let 
  	    )
  	    ( t ( list (SUB-LIST L 0 (/ (+ (length L) 1) 2)) ( SUB-LIST L (+ 1 (/ (- (length L) 1) 2)) (/ (-(length L) 1)2) )))
	);end cond
);end fun

; 6 Write a single LISP function, called BTREE-HEIGHT, which takes a binary tree TREE, 
; and returns the height of TREE Note that the height of a binary tree is defined as the 
; length of the longest path from the root node to the farthest leaf node

;; BTREE-HEIGHT(TREE)
;; INPUT :: 
	; TREE -> binary tree in list form (L,R) where L is left child and R is right child
;; OUTPUT ::
	; integer 0 to N of max height of the tree
;; Algorithm ::
	; Base Case if the Tree is null or only one element return 0
	; Else return 1+ Max( left_sub_tree_heigth , right_sub_tree_height)


( defun BTREE-HEIGHT (TREE) 
 	(cond ((or (null TREE)(atom TREE)) 0) 
 		  ( (listp TREE)
 		  	(   let ((LH (BTREE-HEIGHT (first TREE))) (RH (BTREE-HEIGHT (second TREE)))) ; var list
 				(cond ((> RH LH) (+ RH 1)) 
 					  (t (+ LH 1)) 
 				) ;end inner cond
 			) 
 		) 
 	) ;end cond
);end fun

; 7. Write a single LISP function, called LIST2BTREE, that takes a non-empty list of atoms LEAVES, and returns a binary tree such that
; - The tree leaves are the elements of LEAVES;
; - For any internal (non-leaf) node in the tree, the number of leaves in its left branch minus the number of leaves in its right branch is 0 or 1.
; For example,
; (LIST2BTREE '(1)) returns 1 (LIST2BTREE '(1 2)) returns (1 2)
; (LIST2BTREE '(1 2 3)) returns ((1 2) 3)
; (LIST2BTREE '(1 2 3 4)) returns ((1 2) (3 4))
; (LIST2BTREE '(1 2 3 4 5 6 7)) returns (((1 2) (3 4)) ((5 6) 7)) 
; (LIST2BTREE '(1 2 3 4 5 6 7 8)) returns (((1 2) (3 4)) ((5 6) (7 8)))
; You can call the function SPLIT-LIST from LIST2BTREE.

; LIST2BTREE
; INPUT ::
; 	LEAVES-> non-empty list of atoms LEAVES
; OUTPUT::
; 	binary tree list in form (L,R), s.t. the number of leaves in its left branch minus the number of leaves in its right branch is 0 or 1
; Algorithm::
; 	check base case if the LEAVES is null return NIL
; 	if only one or two element then return that LEAVES
; 	else split the List and create a list of LIST2BTREE on the first and second half of the split list for intern leaves 

( defun LIST2BTREE (LEAVES)
	(cond ( (null LEAVES) NIL)
		  ( (= (length LEAVES) 1) (car LEAVES))
		  ( (= (length LEAVES) 2) LEAVES)
		  ( t 
		  	( let( (splitList (SPLIT-LIST LEAVES)))
		  		(list (LIST2BTREE (first splitList))(LIST2BTREE (second splitList)))
		  	);end opr t 
		  )

		);end cond
)

; 8. Write a single LISP function, called BTREE2LIST, that takes a binary tree TREE as input, and returns a list of atoms (assume TREE follows the constraints we defined earlier).
; - As the input is a binary tree, each node has at most 2 children;
; - This function is the inverse of LIST2BTREE. That is, (BTREE2LIST (LIST2BTREE X)) = X for all lists of atoms X.
; For example,

; (BTREE2LIST 1) returns (1) (BTREE2LIST '(1 2)) returns (1 2)
; (BTREE2LIST '((1 2) 3)) returns (1 2 3)
; (BTREE2LIST '((1 2) (3 4))) returns (1 2 3 4)
; (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) returns (1 2 3 4 5 6 7) 
; (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) returns (1 2 3 4 5 6 7 8)

; BTREE2LIST (TREE)
; INPUT ::
; 	TREE-> binary tree list in form (L,R)
; OUTPUT::
; 	LEAVES-> non-empty list of atoms LEAVES
; Algorithm::
; 	check base case if the TREE is null return NIL
; 	if only one or two element then return that LEAVES by makeing the atom in list form
; 	else append the lists from BTREE2LIST on first and second half of the TREE
( defun BTREE2LIST (TREE)
	( cond  ( (null TREE) NIL)
			( (atom TREE) (list TREE))
			( t (append (BTREE2LIST (car TREE)) (BTREE2LIST (cdr  TREE))))
	);end cond

); end fun 

; 9. Write a single Boolean LISP function, called IS-SAME, 
; that takes two LISP expressions E1 and E2 whose atoms are all numbers, 
; and checks whether the expressions are identical. 
; In this question, you can only use ‘=‘ to test equality (you cannot use ‘equal’).
; Recall that a LISP expression is either an atom or a list of LISP expressions.
; For example,
; (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) returns T
; (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) returns NIL

; IS-SAME (E1 E2)
; INPUT ::
; 	E1 -> first Lisp Expr list whose atoms are all numbers
;   E2 -> second Lisp Expr list whose atoms are all numbers
; OUTPUT::
; 	t if E1 and E2 is identical else nil
; Algorithm::
; 	check base case if E1 is null return if E2 is null 
;   check base case if E1 and E2 are both list and have different length return Nil
;   if both E1 and E2 only have one atom and they are number return if they are equal
; 	else check if the both the first element of the E1 and E2 are identiacal and the rest of E1 and E2 are identical with IS-SAME recursivelly 

( defun IS-SAME (E1 E2)
	(cond
		( (null E1) (null E2))
		( (and (listp E1)(listp E2)(not(= (length E1) (length E2)))) NIL)
		( (and (numberp E1) (numberp E2)) (= E1 E2))
		(t(and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))) )
	);end cond
);end fun 






