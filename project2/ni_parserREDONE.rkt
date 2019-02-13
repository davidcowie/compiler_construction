#lang racket

(require "lexicalAnalysis.rkt")
(require (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require (prefix-in lex: parser-tools/lex))
(require "ni_ast.rkt")

(provide (all-defined-out))

;(lexstr "(15+7) and hello")


; cfg-parser!!1
; src-pos is in there
(define niparser
  (parser
   ;(src-pos)
   (start statements) ; changed this to expr and then it worked...
   (end EOF)
   (tokens value-tokens paren-types operators punctuation comparators boolops keywords endoffile booleanToks)
   ; Getting error with this error function, keeps expecting 5 params but is only getting 3..
   ; could it have something to do with the src-pos???
   #|
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (lex:position-line start-pos) (lex:position-col start-pos) tok-name tok-value tok-ok?)))) ; put in that ERROR thing from the assignment!!!
|#
   (error (lambda (tok-ok? tok-name tok-value)
            (if (and (eq? tok-ok? #t) (eq? tok-name 'EOF)) '()
                (printf "Parsing error at line ~a, col ~a: token: ~a, value: ~a, tok-ok? ~a\n"
                        (+ 0 0) (+ 0 0) tok-name tok-value tok-ok?)))) ; put in that ERROR thing from the assignment!!!
   (grammar
    ;(prog [(statements) (program (flatten (filter (λ (el) (not (null? el))) $1)))]) ; from DBN parser
    (statements [(expr) (list $1)])
                ; added these two part when add the newline into the lexer
          ;[(expr newlines) (list $1)]
          ;[(expr newlines statements) (cons $1 $3)])
    ;(newlines [NEWLINE ; don't have new line tokens
    ; CHECK IF CAN HAVE DECLARATIONS LUMPED IN WITH EXPRESSIONS!!
    (exprs
     [(expr) (list $1)]
     ;[(expr newlines exprs) (cons $1 $3)]
     )
    ; perhaps separate expr from statements (to make distinction between expr and declarations or anything.
    
    (expr
          [(declaration) $1] ; ADDED AFTER MAKING THE THING PLURAL
          [(declarations) $1] ; can be a declaration CHANGED TO DECLARATIONS PLURAL!!! WHICH NOW RETURNS A LIST
          [(declaration AND declaration) (cons $1 $3)] 
          [(lvalue) (begin (print "lvalue") $1)]
          
          ;[(expr math-operator expr) (MathExpr $1 $2 $3)] ; arithmetic -- Note: says should only be with numbers, but that would be a type checking phase i think.
          ; WONDERING IF SHOULD PUT THIS INTO ITS OWN GROUP
          ; currently no way of type checking the first term in the add or sub expression. or term in a multiplication, because they all loop back to be an expression.
          ;[(expr ADD term) (MathExpr $1 '+ $3)]
          ;[(expr SUB term) (MathExpr $1 '- $3)]
          ;[(term) $1]

          [(expr bool-operator boolTerm) (BoolExpr $1 $2 $3)]
          [(boolTerm) $1]

          
          
          ; IF IN OWN GROUP COULD BUILD PRECEDENCE OF THESE OPERATORS INTO IT AS WELL.
          ;[(expr bool-operator expr) (BoolExpr $1 $2 $3)] ; Boolean Comparisions.
          ;[(expr logic-operator expr) (LogicExpr $1 $2 $3)] ; Logic operators.
          
          #|
           These are the literal values, could put them into there own category.
          |#
          [(STRING) (StringExpr $1)] ; string literal
          [(NUM) (NumExpr $1)] ; number literal
          [(ID) (VarExpr $1)] ; variable expression. Maybe should move into an lvalue
          
          ;[(functiondeclaration) $1] ; function declaration
          
          [(LPAREN RPAREN) (NoVal)] ; no value expression.
          [(LPAREN expr RPAREN) $2]
          [(LPAREN seq-expr RPAREN) $2] ; parenthesis evaluate
          ;[(LPAREN expr SEMI expr RPAREN) (cons $2 $4)] ; sequences
          ; why am i getting a list*????
          ;[(expr SEMI expr) (cons $1 $3)] ; sequences cont.
          ;[(SUB NUM) (MathExpr (NumExpr "0") '- (NumExpr $2))] ; negation, What symbol should I use?
          [(ID LPAREN RPAREN) (FuncallExpr $1 '())] ; function call, no arguments.
          [(ID LPAREN func-args RPAREN) (FuncallExpr $1 $3)] ; function call with arguements NOTE:: Changed to expr from typefields, cause in function call, don't specify the type.
          [(type-id LBRACE field-assign RBRACE) (NewRecordExpr $1 $3)]  ; record creation
          ;========================================
          ; APPARENTLY WHEN COMMENT THIS OUT I CAN JUST USE LIKE COL[A] BUT WHEN ITS HERE IT DOESN'T GET THAT THAT IS AN LVALUE AND IT THROWS AN ERROR CAUSE ITS LOOKING FOR THE OF
          ; SO APPARENTLY IT ISN'T BACK TRACKING, NOR IS IT TRYING THE FIRST THING IT COMES ACROSS.
          ; BECAUSE IT WORKS FOR SOME REASON WITH THE RECORD, WHICH IS SLIGHTLY DIFFERENT...
          ; =========================================
          ; BUT WHEN I ADDED THIS EXPLICITLY RIGHT HERE (JUST BELOW)) IT THEN WORKS!!! SO DON'T GET THAT.
          ; DOESNT WORK IF REPLACE ID with lvalue!!! so thinking issue is with lvalue.. maybe there, its mathing too mcuh...
          ; the array accessing seems to work everywhere except when its by itself....
          ; And if leave this in here, then never seems to get a newArrayExpr
          ; (parse-str "with j as 0 to N - 1 do
        ;print(if col[i] = j then 5 else 6) end") This works with the bottom line included.
          ;[(ID LBRACKET expr RBRACKET) (ArrayExpr $1 $3)]
          ; should both of the type-id's be lvalues?
          ;[(type-id LBRACKET expr RBRACKET OF expr) (NewArrayExpr $1 $3 $6)] ; array creation
          ;[(type-id LBRACKET expr RBRACKET) (ArrayExpr $1 $3)]
          ; with both lvalues, it then seems to work for all cases..
          ; but!!! WITH THE ARRAY CREATION!! DON'T WANT A VAREXPR FOR THE ID, JUST WANT THE RAW STRING!!!
          ; CURRENTLY GET A VAREXPR, but for the array expr, do I want a varexpr??
          ; DON'tTHINK HAVE THAT ANSWER!!
          [(lvalue LBRACKET expr RBRACKET OF expr) (NewArrayExpr $1 $3 $6)] ; array creation
          [(lvalue LBRACKET expr RBRACKET) (ArrayExpr $1 $3)]
          ;===
          ;[(IF expr THEN expr ELSE expr END) (IfThenExpr $2 $4 $6)] ; if then else statement.
          ;====
          ;[(IF expr THEN expr END) (IfExpr $2 $4 #f)] ; if then statement. Note: not sure how handle no false branch.. just assuming doing a false for now.
          ;====
          ;[(WHILE expr DO expr END) (WhileExpr $2 $4)] ; while loop.
          ; NEED ASSIGNMENT EXPR
          ; up
          [(NOW lvalue IS expr) (AssignmentExpr $2 $4)] ; assignment expression. Note: might want to add in an lvalue terminal node. updated to use lvalue
          ;====
          ;[(WITH ID AS expr TO expr DO seq-expr END) (WithExpr $2 $4 $6 $8)] ; With expression. Note: seems to have a different form in ast definition and documentation explination.
          [(BREAK) (BreakExpr)] ; Break expression
          ; NOT SO SURE ABOUT THIS ONE. NEED TO MOVE DECLARATIONS
          ; Guess also should have declarations be continuous. didn't specify the form of how that looks...
          ; wants list of declarations and list of expressions.
          #|
NEED TO KNOW HOW THE DECLARATIONS AND EXPRESSIONS ARE SEPARATED IN THE LET STATEMENT. WHAT IS THE SYNTAX OF THE SEPARATION.
CAUSE COULD BE NEWLINES LIKE IN DBN, BUT DON'T HAVE A TOKEN FOR THAT RIGHT NOW. SO THAT PART ISNT FULLY CORRECT.
|#
          ;====
          ;[(LET declarations IN seq-expr END) (LetExpr $2 $4)] ; Let expression -- using seq-expr because that matches to expressions that are separated by semicolons
          ;====
          ;[(LET declarations IN END) (NoVal)] ; Said this does not yield a value... so assume just ignore all of it...

          ; Update for the things ending in end
          [(thingsWithEnd END) $1]
          [(thingsWithEnd) $1]
          
          ;[(LET declarations IN expr) (LetExpr $2 $4)] ; LEt expression
          ;[(LET declarations IN expr) (LetTestExpr $2)]
          )
    (thingsWithEnd
     [(LET declarations IN seq-expr) (LetExpr $2 $4)] ; Let expression -- using seq-expr because that matches to expressions that are separated by semicolons
     [(LET declarations IN) (LetExpr $2 '())] ; Said this does not yield a value... so assume just ignore all of it...
     [(WITH ID AS expr TO expr DO seq-expr) (WithExpr $2 $4 $6 $8)] ; With expression. Note: seems to have a different form in ast definition and documentation explination.
     [(IF expr THEN expr ELSE expr ) (IfExpr $2 $4 $6)] ; if then else statement.
          [(IF expr THEN expr ) (IfExpr $2 $4 #f)] ; if then statement. Note: not sure how handle no false branch.. just assuming doing a false for now.
          [(WHILE expr DO expr ) (WhileExpr $2 $4)] ; while loop.
          )

    
    ; THE NEW LINES ISNT REALLY RIGHT. BUT THIS IS A THOUGHT OF HOW TO HAVE MULTIPLE DECLARATIONS LIST FORM
    ; WHEN NO SPECIFICATION ON HOW THEY SHOULD BE SEPARATED!!!!
    ; So, can remove the newlines stuff, and just have them one after another. Then itll just put them all into a list.
    (declarations
     [(declaration) (list $1)]
     ;[(declaration newlines declarations) (cons $1 $3)]
     [(declaration declarations) (cons $1 $2)]
     [(declaration AND declarations) (cons $1 $3)]
     )
    ; Currently not in use.
    (newlines
     [(NEWLINE) null]
     [(NEWLINE newlines) $2])
    
    (declaration
     [(DEFINE type-id KIND AS type-id) (NameType $2 $5 '())]
     [(DEFINE type-id KIND AS LBRACE typefields RBRACE) (RecordType $2 $6 '())] ; record type declaration. -- CHANGED FROM ty to typefields
     [(DEFINE type-id KIND AS ARRAY OF type-id) (ArrayType $2 $7 '())] ; array type declaration
     [(NI ID IS expr) (VarDecl #f $2 $4)] ; variable declaration, no type.
     ; possibly should use different node names to prevent certain constructs. like with IS expr, could have a define after? maybe thats ok?
     [(NI type-id ID IS expr) (VarDecl $2 $3 $5)] ; variable declaration
     [(functiondeclaration) $1] ; function declaration
     )
    
    ; BOOLEAN LITERALS!!!!
    ; SHOULD I START USING THE MATH SYMBOLS NOW??
    (math-operator
     [(ADD) '+]
     [(SUB) '-]
     [(MULT) '*]
     [(DIV) '/])

    (boolTerm
     [(boolTerm logic-operator logicalTerm) (LogicExpr $1 $2 $3)]
     [(logicalTerm) $1]
     )
    (logicalTerm
     [(logicalTerm ADD term) (MathExpr $1 '+ $3)]
     [(logicalTerm SUB term) (MathExpr $1 '- $3)]
     [(term) $1]
     )
    (term
     [(term MULT factor) (MathExpr $1 '* $3)]
     [(term DIV factor) (MathExpr $1 '/ $3)]
     [(factor) $1])
    (factor
     [(rvalues) $1]
     [(SUB rvalues) (MathExpr (NumExpr "0") '- $2)] ; negation, What symbol should I use? ADDED THIS DOWN HERE CAUSE IT HAS THE HIGHEST PRECEDENCE
     [(LPAREN expr RPAREN) $2]) ; with this, can have a variable declaration within math expressions.
    (rvalues
     [(NUM) (NumExpr $1)]
     [(ID) (VarExpr $1)])
    ;(EQ NE LT GT LE GE)
    ; on small sample tests, uses symbol 'eq for equal. HOW AM I SUPPOSED TO KNOW WHAT SYMBOLS???
    (bool-operator
     [(EQ) 'eq]
     [(NE) 'ne]
     [(LT) 'lt]
     [(GT) 'gt]
     [(LE) 'le]
     [(GE) 'ge])
    (logic-operator
     [(BOOLOR) 'or]
     [(BOOLAND) 'and])

    ; Looks like the inside of the function is formed as a sequence. So that same format.
    (functiondeclaration
     ; still unsure how to handle the NEXT part. so for now just doing all empty lists.
     ; i think it has something to do with using AND but not sure.
     ; Should the inside of functions have multiple expressions?
     [(NEEWOM ID LPAREN typefields RPAREN IS expr) (FunDecl $2 $4 #f $7 '())]
     [(NEEWOM ID LPAREN typefields RPAREN AS type-id IS expr) (FunDecl $2 $4 $7 $9 '())]
     ; guess need case for no parameters, cause don't know how to have typefields match nothing...
     [(NEEWOM ID LPAREN RPAREN IS expr) (FunDecl $2 '() #f $6 '())]
     [(NEEWOM ID LPAREN RPAREN AS type-id IS expr) (FunDecl $2 '() $6 $8 '())]
     )

    ; the typefields are for like records, can be a list that goes on indefinitely. So guess I could say its a ty?
    ; but it would need a comma?...
    (typefields [(type-id ID) (cons (TypeField $2 $1) '())] ;; was lowercase id CHANGED TO CONS WITH EMPTY BECAUSE FAILED WITH RECORD TYPE
                [(type-id ID COMMA typefields) (cons (TypeField $2 $1) $4)])
    ; weird that field assign is always a list, whereas typefields is only a list if there is more than one of them...
    (field-assign
     [(ID IS expr) (cons (FieldAssign $1 $3) '())]
     [(ID IS expr COMMA field-assign) (cons (FieldAssign $1 $3) $5)])
    (func-args
     [(expr) (cons $1 '())]
     [(expr COMMA func-args) (cons $1 $3)])
    ; MIGHT NEED TO ADD NEW LINES SOMEWHERE INTO HERE, LIKE AFTER THE SEMI COLON TO INDICATE THAT NEW LINES ARE OK IN SEQUENCES!!!!
    (seq-expr
     [(expr) (cons $1 '())]
     [(expr SEMI seq-expr) (cons $1 $3)])
    (type-id [(ID) $1]) ; VarExpression?? When made it a var expression, it got messy
    ; DIFFERENCE BETWEEN AN L VALUE aND type-id?? seemed to be used similarly
    ; This mainly seems to be for reading a value.
    (lvalue
     [(ID) (VarExpr $1)] ; just an id, i assume a regular variable. Like x=5, then (x) to get the value of x
     [(lvalue DOT ID) (RecordExpr $1 $3)] ; the dot notation is to access an item in a record. NOTE:: Not sure if this should be a record expression or what???
     ;[(lvalue LBRACKET expr RBRACKET) (ArrayExpr $1 $3)]
     [(lvalue LBRACKET expr RBRACKET) (ArrayExpr $1 '())]
     )
    )))



; input port -> 0-arg function that returns next token
;(define (get-tokenizer in)
;  (lambda () (lexer in)))

; input port -> ni ast   
; construct an ast from the input port
(define (build-ast in)
  (port-count-lines! in)
  (niparser (get-tokenizer in)))

; string representing ni code -> ni ast
; parses a string and turns it into an ast if possible
(define (parse-str str)
  (let ([in (open-input-string str)])
    (begin (print (lexstr str))
    (build-ast in))
    )
  )

; string (filename) -> ni ast
; opens and parses a file and tries to turn it into an ast if possible
(define (parse-file filename)
  (let ([in (open-input-file filename)])
    (build-ast in)))
