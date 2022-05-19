;
; CS161 Spring 2020 HW6 Problem 3: Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw6.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
; var-index@(n,c)= (n-1)*k+c
; Input 
;   n: Node Index Number 
;   c: color Index number 
;   k: Number of color 
; Output
;   Unique var index meaning var-index@(n,c)= (n-1)*k+c

; Alg: 
;   Hashing with the convestion (n-1)*k+c
(defun node2var (n c k)
  (+ (* (- n 1) k) c) 
);end fun 


; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
; (I(1,1)I(1,2)I(1,3)...)

; Input 
;   n: Node Index Number 
;   c: color Index number 
;   k: Number of color 
; Output
;   a list of all var index number of Node n with assign color c from {c...k}
; Alg: 
;    ( red or blue or green ..... )
;    recusiivly and call node2var from c=1 to c=k and make a list
(defun at-least-one-color (n c k)
  ( if(> c k)nil
    (append (list (node2var n c k) ) (at-least-one-color n (+ c 1) k) )
  );end if
);end fun 

; Input 
;   curr: current color index chosen 
;   allcolor: the list of all possible color 
; Output
;   a list of list which reprsents Node(x, color) -> ~Node(x, y) ^ y != color
;    (red and ~g and ~ b ....)( g and ~ r and ~b ....)
; Alg: 
;    (red and ~g and ~ b ....)( g and ~ r and ~b ....)
;    make a permuation list and negatate the pair since p-> ~q is ~p v ~q base on
;     the current cosen color curr with the list allcolor

(defun make-not-same-color( curr allcolor)
  ( cond
    ((null allcolor)nil)
    ((= (length allcolor) 1)(list (list (* curr -1) (* (car allcolor) -1))))
    (t( let(( nc2 (car allcolor)));end my var list 
      ( append (list (list (* curr -1) (* nc2 -1)))(make-not-same-color curr(cdr allcolor)))
      );end let
    );end else
  );end cond
);end fun
( defun make-permu-list( allcolor)
  (make-not-same-color (car allcolor) (cdr allcolor))
)


; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."

; Input 
;   n: Node Index Number 
;   c: color Index number 
;   k: Number of color 
; Output
;   ; varIndex(1, R)-> ~varIndex(1, G)^~varIndex(1, B)^...
;   ~p v ~q : ~varIndex(1, R) v ~varIndex(1, x) ^ (x != R)
;    (~R v ~G)(~R v ~B)......
;    a list of list which reprsents Node(x, color) -> ~Node(x, y) ^ y != color
;    (red and ~g and ~ b ....)( g and ~ r and ~b ....)
; Alg: 
;    recusiivly and call make-not-same-color from c=1 to c=k of node index n with max color k
(defun at-most-one-color (n c k)
   ( if (> c k)nil
    ( let( (mycolors (at-least-one-color n c k) ))
      (append(make-permu-list mycolors) (at-most-one-color n (+ c 1) k)
      )
    )
  );end if
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; Input 
;   n: Node Index Number 
;   k: Number of color 
; Output
; "node n gets exactly one color from the set {1,2,...,k}."
; full fill node n with colors k can st n get asssign at least one color from [c, k] and only one color is being assigned 
(defun generate-node-clauses (n k)
  (append (list (at-least-one-color n 1 k)) (at-most-one-color n 1 k))
)

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
; Input
;   v1 v2: the variables of the currrent edge 
;   c: color Index number 
;   k: Number of color 
; Output
;   make a list of list st both color that are not same color 
; Alg
;   p : v1 assign curr color c
;   q : v2 assign curr color c
;   p->~q = ~p v ~q 

( defun make-sat-edge-btw ( v1 v2 c k )
  (list (list (* (node2var v1 c k) -1) (* (node2var v2 c k) -1)))
)


; Input
;   e:the current edge 
;   k: Number of color 
; Output
;   make a list of list st  the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}
; Alg
;   p : v1 assign curr color c
;   q : v2 assign curr color c
;   p->~q = ~p v ~q 
;    recusiivly and call make-sat-edge-btw  from c=1 to c=k of node index n with max color k on edge e

(defun generate-edge-clauses (e k)
  (defun make-edge-clause (e c k)
    (if(or (null e) (> c k)) nil
      (append (make-sat-edge-btw (first e)(second e) c k)(make-edge-clause e (+ c 1) k))
    )
  );end recur 
  ( if( null e)nil
    (make-edge-clause e 1 k)
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
; 
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
;
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
;
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
