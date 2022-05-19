

; this is the function that check if the current literal 
; indeed makes match with the one of the asssigment  
; with the onservation that
; if both the current assignmetn is ineed
; if X is X then true  
; if X and -X then false so X* -X will gice us negative number 

(defun is-literal-assigned (literal curr_assignments)
  ( let 
    (( curr_assign (car curr_assignments)));end of of my var lsit 
    (if(null curr_assignments) t
      ( if( = (abs literal) (abs curr_assign))
        ( = curr_assign literal);end t
        (is-literal-assigned literal (cdr curr_assignments));else 
      );end if if 
    );end cond
  ); end let
);end fun 

; is-calues-SAT is the helper funciton of is-Delta-Sat 
; which check againt each calue from the list
; of claues with assignment, and if any lital within 
; the current clause 
; input
;   curr_clause-> the current clause being exmain from the delta list
;   curr_assignment -> the assignment retirived so far 
; output
;   t if the literal within the current clause indeed has match 
;   nil if not mwatch 

(defun is-clause-sat (curr_clause curr_assignments)
    (cond 
        ((null curr_clause) NIL) 
        (t 
          (let ((curr_literal (car curr_clause) )) ;end my var lsit 
            (cond 
                ((is-literal-assigned curr_literal curr_assignments) t) 
                (t (is-clause-sat (cdr curr_clause) curr_assignments))
            );end cond
          );end let 
        );end cond 
    );end cond
);end fun

; IS Delta will be checking if one of the Assignment
; makes satify the SAT CNF problem
; input ->
;   delta -> CNF claues
;   curr_assignment -> the assignment retirived so far 
; output->
;   t if any 

(defun is-Delta-Sat (delta curr_assignment)
    (cond
        ((null delta) t) ; base case
        (t (and 
          (is-clause-sat (car delta) curr_assignment) 
          (is-Delta-Sat  (cdr delta) curr_assignment)))
    );end cond
);end fun 
;; helper function that is in assitnatnt for makeAssignmen
;; that help to change the sign of the literal, if one 
;; of the assignmetn dosnt make the claues SAT 
;

( defun setAssignemtonlit( sign m_assign ) 
  (* sign (+ (length m_assign) 1))
);end fun
;; creat an Assignment list structure along with the backtrack path  
;; and keep apend when a new element is being added 
(defun makeAssignment (curr_assign sign)
    (cons (setAssignemtonlit sign curr_assign ) curr_assign)
);end fun 


;;; this function is mainly for backtrach search physics
; input
;   n -> the number of var1
;   delta -> the CNF caluese list of list 
;   curr_assignment -> the assignment retirived so far 
; output
;   a list of assignmet or nil of cant be found 
; alg:
;; if assigning true to the current first literal of the current delta cluse works
;   ->then simply return it 
; else try to assiggn false to the current first literal of the current delta cluse 
;   ->if it works return it
; else
;    there is no soultion too bad !!!


;;; this is initaled the serach by assning the literals
( defun try-backtrack-search(n delta curr_assignment)
  (or (serach-with-backtrack n delta (makeAssignment curr_assignment 1)) 
          (serach-with-backtrack n delta (makeAssignment curr_assignment -1)))
);end fun
;;; this function is mainly for check backtrach search 
; input
;   n -> the number of var1
;   delta -> the CNF caluese list of list 
;   curr_assignment -> the assignment retirived so far 
; output
;   a list of assignmet or nil of cant be found 
; alg:
  ; check if the assignment is indeed satisfy the constrian 
  ; if the ineed satisfy and the element is matched with n 
  ; -> we have the soultion 
  ; else if the assignemt is satisfy the constrian so far 
  ; -> we start another serach 
  ; else if the the assignemt doesnt satisfy the constrian
  ; ->nil

;   
( defun check-along-backtrack(n delta curr_assignment)
  (let( ( isSat (is-Delta-Sat delta curr_assignment) )) 
        (cond 
          ( (and isSat (= (length curr_assignment) n) ) curr_assignment)
          (  (not isSat) nil)
          (t (try-backtrack-search n delta curr_assignment ))
        );end cond
     );else case 
);end fun
;;; this function is mainly for backtrach search 
; input
;   n -> the number of var1
;   delta -> the CNF caluese list of list 
;   curr_assignment -> the assignment retirived so far 
; output
;   a list of assignmet or nil of cant be found 
; alg:
;   if the current assignmetn is null, start trying to
;   backtrack serach 
;   if the asssignmet is already being created check if the 
;   assignemnt is indeed valid 

(defun serach-with-backtrack (n delta curr_assignment )
  ( if ( null curr_assignment)
     (try-backtrack-search n delta curr_assignment)
     (check-along-backtrack n delta curr_assignment)
  );end if 
);end fun

; CNF sat solver that utilizes backtrack search
(defun sat? (n delta) 
    (reverse(serach-with-backtrack n delta nil))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
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

