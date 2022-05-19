;hw1 test script

(load "hw1.lsp")
(defun my_test ()
    (if (and 
            (equal (TREE-CONTAINS 3 '((1 2 3) 7 8)) T)
            (equal (TREE-CONTAINS 4 '((1 2 3) 7 8)) nil)

            (equal (TREE-MIN '((1 2 3) 7 8)) 1)

            (equal (TREE-ORDER 3) '(3))
            (equal (TREE-ORDER '((1 2 3) 7 8)) '(7 2 1 3 8))
            (equal (TREE-ORDER '(((1 5 3) 9 10) 11 (12 13 (17 18 20)))) '(11 9 5 1 3 10 13 12 18 17 20))
            
            (equal (SUB-LIST '(a b c d) 0 3) '(a b c))
            (equal (SUB-LIST '(a b c d) 3 1) '(d))
            (equal (SUB-LIST '(a b c d) 2 0) nil)

            (equal (SPLIT-LIST '(a b c d)) '((a b) (c d)))
            (equal (SPLIT-LIST '(a b c d e)) '((a b c) (d e)))
            (equal (SPLIT-LIST '(a b c d e f)) '((a b c) (d e f)))

            (equal (BTREE-HEIGHT 1) 0)
            (equal (BTREE-HEIGHT '(1 2)) 1)
            (equal (BTREE-HEIGHT '(1 (2 3))) 2)  
            (equal (BTREE-HEIGHT '((1 2) (3 4))) 2)
            (equal (BTREE-HEIGHT '((1 (2 3)) ((4 5) (6 7)))) 3)
            (equal (BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8)))) 3)

            (equal (LIST2BTREE '(1)) 1) 
            (equal (LIST2BTREE '(1 2)) '(1 2)) 
            (equal (LIST2BTREE '(1 2 3)) '((1 2) 3)) 
            (equal (LIST2BTREE '(1 2 3 4)) '((1 2) (3 4))) 
            (equal (LIST2BTREE '(1 2 3 4 5 6 7)) '(((1 2) (3 4)) ((5 6) 7))) 
            (equal (LIST2BTREE '(1 2 3 4 5 6 7 8)) '(((1 2) (3 4)) ((5 6) (7 8))))

            (equal (BTREE2LIST 1) '(1)) 
            (equal (BTREE2LIST '(1 2)) '(1 2)) 
            (equal (BTREE2LIST '((1 2) 3)) '(1 2 3)) 
            (equal (BTREE2LIST '((1 2) (3 4))) '(1 2 3 4)) 
            (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) 7))) '(1 2 3 4 5 6 7)) 
            (equal (BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8)))) '(1 2 3 4 5 6 7 8))

            (equal (IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) T)
            (equal (IS-SAME '(1 2 3 7 8) '((1 2 3) 7 8)) nil)
            (equal (IS-SAME '(1) '(1)) T)
            (equal (IS-SAME 1 1) T)
            (equal (IS-SAME '() '()) T)
            (equal (IS-SAME '(1 "a") '(1 "a")) nil)

        )
        "pass"
        "fail"
    )
)