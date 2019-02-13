#lang racket

#|
Test file for the Parser.

The first section just includes basic stand alone parse-str examples, used just to verify that no error is occuring with any of those.
Then I switch to testing the parser against all of the test files, various functions to do that.
    The files are tested to make sure no errors occur, and then there are check-expects for the ni files that I believe are 'correct'
Finally it uses the tests that were given in the assignment description.

ni_ast.rkt file just contains all of my definitions of the expression structs.
|#
(require "ni_ast.rkt" test-engine/racket-tests)
(require "ni_parser.rkt")


;;; Basic parse-str tests.
(parse-str "5+3")

(parse-str "ni x is 5")

(parse-str "define int2 kind as int")
(parse-str "define intarr kind as array of int")
(parse-str "define intrec kind as { int x }")
(parse-str "neewom getX() as int is 5")

(parse-str "(5;6;5)")
(parse-str "point { x is 6, y is 7 }")
(parse-str "add2(5,6)")

(parse-str "ni low-value is 5")
(parse-str "ni int low-value is 5")

(parse-str "define point kind as { int x, int y }")
(parse-str "define intlist kind as {int hd, intlist tl}")

(parse-str "((5;6))")

(parse-str "define tree kind as {int key, treelist children} and
define treelist kind as {tree hd, treatlist tl}")

(parse-str "(now row[r] is 1; now diag1[r + c] is 1; now diag2[r + 7 - c] is 1;
                       now col[c] is r;
                      now row[r] is 0; now diag1[r + c] is 0; now diag2[r + 7 - c] is 0)")


; changed from now try(c+1)
(parse-str "(now row[r] is 1; now diag1[r + c] is 1; now diag2[r + 7 - c] is 1;
                       now col[c] is r;
                       try(c + 1);
                       now row[r] is 0; now diag1[r + c] is 0; now diag2[r + 7 - c] is 0)")

(parse-str "ni low-value is 5
ni int low-value is 5")

; These are some of the different cases
(parse-str "col[a] of b")
(parse-str "col[a]")
(parse-str "col.y[a]")
(parse-str "col.y[a] of b")

; ni row is intArray [ N ] of 0  ; the part after the 'is' is an array creation.
; so it is saying that the variable row, is an array with 9 elemets, each one initialized to a 0
(parse-str "ni row is intArray [ N ] of 0")
(parse-str "let
  ni N is 9

  define intArray kind as array of int

  ni row is intArray [ N ] of 0 
  ni col is intArray [ N ] of 0
  ni diag1 is intArray [ N + N - 1] of 0
  ni diag2 is intArray [ N + N - 1] of 0
in try(0)
end")

(parse-str "neewom printboard () is 
    (with i as 0 to N - 1 do
      (with j as 0 to N - 1 do
        print(if col[i] = j then \" 0\" else \" .\");
        print(\"\\n\"));
      print(\"\\n\"))")

(parse-str "with j as 0 to N - 1 do
        print(if col[i] = j then 5 else 6) end")


(parse-str "neewom try (int c) is
    if c = N - 1
    then printboard()
    else with r as 0 to N - 1 do
              if row[r] = 0 & diag1[r + c] = 0 & diag2[r + 7 - c] = 0 
              then (now row[r] is 1; now diag1[r + c] is 1; now diag2[r + 7 - c] is 1;
                       now col[c] is r;
                       
                       now row[r] is 0; now diag1[r + c] is 0; now diag2[r + 7 - c] is 0)")


;;; operator precedence checks
(check-expect (parse-str "5*5+3*5") (list (MathExpr (MathExpr (NumExpr "5") '* (NumExpr "5")) '+ (MathExpr (NumExpr "3") '* (NumExpr "5")))))
(check-expect (parse-str "5*(3+2)") (list (MathExpr (NumExpr "5") '* (MathExpr (NumExpr "3") '+ (NumExpr "2")))))
(check-expect (parse-str "5*(3+2)&7") (list (LogicExpr (MathExpr (NumExpr "5") '* (MathExpr (NumExpr "3") '+ (NumExpr "2"))) 'and (NumExpr "7"))))
(check-expect (parse-str "5*(3+2)&7*-3+1") (list (LogicExpr (MathExpr (NumExpr "5") '* (MathExpr (NumExpr "3") '+ (NumExpr "2")))
                                                            'and
                                                            (MathExpr (MathExpr (NumExpr "7") '* (MathExpr (NumExpr "0") '- (NumExpr "3"))) '+ (NumExpr "1")))))
;;; Added the spacing to make it easier to see the tree.
(check-expect (parse-str "5*(3+2)&7*-3+1 > 15") (list (BoolExpr
                                                       (LogicExpr (MathExpr (NumExpr "5")
                                                                            '*
                                                                            (MathExpr (NumExpr "3")
                                                                                      '+
                                                                                      (NumExpr "2")))
                                                                  'and
                                                                  (MathExpr (MathExpr (NumExpr "7")
                                                                                      '*
                                                                                      (MathExpr (NumExpr "0")
                                                                                                '-
                                                                                                (NumExpr "3")))
                                                                            '+
                                                                            (NumExpr "1")))
                                                       'gt
                                                       (NumExpr "15"))))

; record creation.
(check-expect (parse-str "apples{foo is int, bar is string}") (list (NewRecordExpr "apples" (list (FieldAssign "foo" (VarExpr "int")) (FieldAssign "bar" (VarExpr "string"))))))




;;; File Parsing!
; want to parse all the files
(define test-file-path "ni-p2-tests\\t") ; The relative path the to test folder.
;;; Problems I believe are in some test files:
; 7 doesn't seem to be correct. (L = ni)
; in 11, ni is on its own line with a semi colon, which i don't thinnk should be a valid statement.
; in 18, seems to be a similar issue as 7? also has a comment and uses something = ni which doesn't make sense.
; in 19, uses 'is' in a condition of if statement: 'if y is 4', don't believe that is a valid use of is, should be an equal sign.
; in 20, in the with, has a sequence expression,  and the second thing is just "x is x + 1", but I believee that should be "now x is x + 1" as I don't see is being used on its own in the documentation.
; in 21, get a parsing error on "(j[0] is indexedString { index is 2, value is "bo" }; 0 )" line. Cannot continue after 'is'. Again, I believe this is an incorrect use of 'is'
;      the 'is' is never preceded only by an id unless it is for a record creation.
; in 22, there are extra statements after the let ends. I thought it was said that shouldn't be legal. It should all fit in as a statement. not just have loose statement one after the other.


;;; This tests all the files in the test folder. only makes sure there aren't any errors.
;;; The invalid-list are the files that I believe have issues with them and need to be corrected, which is the point of the next function.
(define (test-files fileNum)
  (let ([invalid-list (list 7 11 18 19 20 21 22)]);(list 7 11 18 19 20 21 22)])
  (cond [(= fileNum 24) (displayln "All done with correct given files.\n")]
        [(member fileNum invalid-list) (begin (displayln (string-append (number->string fileNum) ":\nFile Contains Error\n")) (test-files (+ fileNum 1)))]
        [else (let [(ast (parse-file (string-append test-file-path (number->string fileNum) ".ni") #f))]
           (begin (print fileNum) (display ": \n") (println ast) (display "\n") (test-files (+ fileNum 1)))
         )])))

; this goes through the corrected files.
(define (test-fixed-files flst)
  (cond [(empty? flst) (displayln "Finished Corrected Files.")]
        [else
         (let [(ast (parse-file (string-append "ni-p2-test-corrected\\t" (number->string (first flst)) ".ni") #f))]
           (begin (print (first flst)) (display ": \n") (println ast) (display "\n") (test-fixed-files (rest flst))))])
  )

;;; This will go through all the test files. Including the corrected files that I put in a new Folder.
;;; The point of this function is to quickly check that you don't get any parsing errors with any of the files.
(define (test-all-files)
  (displayln "Parsing given files.\n")
  (test-files 1)
  (displayln "Starting corrected Files")
  (test-fixed-files (list 11 19 20 21 22)))

;;; No clue how to fix 18 with it still making sense with how it was intended.
;;; Same issue with 7


; This is a list of correct parsed output for the files.
; this is a list of pairs where the first value is the file number, and the second is the expected output.
; Didn't write expected outputs for my corrected test files because wan't to make sure my corrections are in fact correct.
(define lstCorrect (list (cons 1 (list (LetExpr (list (NameType "myint" "int" '())) '())))
                         (cons 2 (list (LetExpr (list (NameType "myint" "int" '()) (NameType "mystring" "string" '())) '())))
                         (cons 3 (list (LetExpr (list (NameType "myint" "int" '()) (RecordType "indexString" (list (TypeField "index" "int") (TypeField "str" "string")) '())) '())))
                         (cons 4 (list (LetExpr (list (RecordType "tree" (list (TypeField "key" "int") (TypeField "children" "treelist")) '()) (RecordType "tree" (list (TypeField "key" "int") (TypeField "children" "treelist")) '()) (RecordType "treelist" (list (TypeField "hd" "tree") (TypeField "tl" "treats")) '())) '())))
                         (cons 5 (list (LetExpr (list (VarDecl #f "x" (NumExpr "5")) (VarDecl #f "y" (NumExpr "6"))) '())))
                         (cons 6 (list (LetExpr (list (FunDecl "foo" '() #f (NoVal) '()) (FunDecl "add1" (list (TypeField "x" "int")) "int" (VarExpr "x") '()) (FunDecl "add2" (list (TypeField "x" "int") (TypeField "y" "int")) "int" (MathExpr (VarExpr "x") '+ (VarExpr "y")) '())) '())))
                         ; Skipped 7, 18 as not working.
                         (cons 8 (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (ArrayExpr "myarray" '()) (RecordExpr "foo" "bar")))))
                         (cons 9 (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (FuncallExpr "foo" '()) (FuncallExpr "bar" (list (NumExpr "1") (NumExpr "23") (VarExpr "e")))))))
                         (cons 10 (list (MathExpr (NumExpr "3") '+ (VarExpr "a"))))
                         (cons 12 (list (NumExpr "5")))
                         (cons 13 (list (StringExpr "\"hello world\"")))
                         (cons 14 (list (LetExpr (list (VarDecl #f "lowvalue" (MathExpr (NumExpr "0") '- (VarExpr "a"))) (VarDecl #f "x" (MathExpr (NumExpr "0") '- (NumExpr "1")))) '())))
                         (cons 15 (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (LogicExpr (LogicExpr (VarExpr "a") 'or (VarExpr "b")) 'and (VarExpr "c"))))))
                         (cons 16 (list (LetExpr (list (VarDecl #f "x" (NumExpr "3"))) (list (MathExpr (NumExpr "3") '- (MathExpr (VarExpr "x") '* (MathExpr (VarExpr "y") '+ (NumExpr "2"))))))))
                         (cons 17 (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (LetExpr (list (ArrayType "myarraykind" "int" '()) (VarDecl "myarraykind" "myarray" (NewArrayExpr "myarraykind" (NumExpr "5") (NumExpr "3")))) (list (ArrayExpr "myarray" '())))))))
                         (cons 23 (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (VarExpr "x")))))
                         ))

(define (check-file correct)
  (cond [(empty? correct) #f] ; maybe should be an error.
        [(equal? (parse-file (string-append test-file-path (number->string (car correct)) ".ni") #f) (cdr correct)) #t]
        [else #f]))

(define (check-files correct)
  (let ([numCor 0])
    (define (fun cor num)
      (cond [(empty? cor) numCor]
            [else
             (let ([val (check-file (first cor))])
               (if val
                   (set! numCor (+ numCor 1)) ; probably a better way of doing this...
                   (displayln (string-append "Error in File " (number->string num)))))
             (fun (rest cor) (+ num 1))]))
    (let ([numPassed (fun correct 1)])
    (display "Passed ") (display numPassed) (display "/") (display (length correct)) (displayln " tests."))))


; Use test-all-files to at least check that there aren't any parsing errors. But won't check if have correct output. only checks for no errors.
(test-all-files)
; This will run through the files from the test folder and essentially do a check expect with those.
(check-files lstCorrect)


;;; ALL OF THE GIVEN TESTS.

; var declarations
(check-expect (parse-str "ni x is 5") (list (VarDecl #f "x" (NumExpr "5"))))
; type declarations
(check-expect (parse-str "define int2 kind as int") (list (NameType "int2" "int" '())))
(check-expect (parse-str "define intarr kind as array of int") (list (ArrayType "intarr" "int" '())))
(check-expect (parse-str "define intrec kind as { int x }")
              (list (RecordType "intrec" (list (TypeField "x" "int")) '())))
; function declarations
(check-expect (parse-str "neewom getX() as int is 5")
              (list (FunDecl "getX" '() "int" (NumExpr "5") '())))
; function calls of various sorts
(check-expect (parse-str "add2(5)") (list (FuncallExpr "add2" (list (NumExpr "5")))))
; parens
(check-expect (parse-str "(5)") (list (NumExpr "5")))
; various sequences
(check-expect (parse-str "(6; 5)") (list (list (NumExpr "6") (NumExpr "5"))))
; strings
(check-expect (parse-str "\"Hello World\"") (list (StringExpr "\"Hello World\"")))
; noval
(check-expect (parse-str "()") (list (NoVal)))
; let expressions
(check-expect (parse-str "let ni x is 5 in x end")
              (list (LetExpr (list (VarDecl #f "x" (NumExpr "5"))) (list (VarExpr "x")))))
; math ops
(check-expect (parse-str "1+2")
              (list (MathExpr (NumExpr "1") '+ (NumExpr "2"))))
; math ops using negated numbers
(check-expect (parse-str "-5") (list (MathExpr (NumExpr "0") '- (NumExpr "5"))))

; bool expressions
(check-expect (parse-str "5=6") (list (BoolExpr (NumExpr "5") 'eq (NumExpr "6"))))

; array creation
(check-expect (parse-str "intarr[10] of 6")
              (list (NewArrayExpr "intarr" (NumExpr "10") (NumExpr "6"))))

; record expression
(check-expect (parse-str "point { x is 6 }")
              (list (NewRecordExpr "point" (list (FieldAssign "x" (NumExpr "6"))))))


(test)
