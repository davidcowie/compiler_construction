#lang racket

(require "enviroment.rkt")
(require (prefix-in types: "types.rkt")) ; any function provided by types, now put types:
; will need to require the parser,
(require "ni_parser.rkt")
(require "ni_ast.rkt")

(provide (all-defined-out))

; the typechecker
; its like he is looking at the code for the first time.

; initialize type enviroment to the
;;; empty enviroment and then add the base types.
(define (init-typeEnv)
  (let ([tenv (empty-env)])
    ; type enviroment starts out emtpy, then just extend it with all the base types
    (extend-env tenv 'int (types:make-IntType))
    (extend-env tenv 'string (types:make-StringType))
    (extend-env tenv 'bool (types:make-BoolType))
    (extend-env tenv 'peng (types:make-PengType))
    (extend-env tenv 'void (types:make-VoidType)) ; NOT SURE ABOUT ADDING THIS ONE
    ))

; global variable that is typeEnv that is the enviroment to store all the types.
; the typechecker will set that 
(define typeEnv (make-parameter (init-typeEnv)))

(define (init-typechecker)
  (typeEnv (init-typeEnv))
  )

(define (tc-str str)
  (init-typechecker)
  (let ([ast (parse-str str)])
    (let ([ty (typecheck (first ast) (typeEnv))])
      ty)))

; takes an ast and an enviroment
; look at the parts of the ast, going to recursivly call this to check.
; 
(define (typecheck ast env)
  (let ([type-of-expr ; the type of the expression if going to be the result of doing a match on the ast.
         (match ast
           ;;; Base Type Cases
           [(NumExpr _) (types:make-IntType)]
           [(StringExpr _) (types:make-StringType)]
           [(BoolVal _) (types:make-BoolType)]
           [(PengExpr) (types:make-PengType)]
           [(MathExpr e1 op e2) (let ([t1 (typecheck e1 env)]
                                      [t2 (typecheck e2 env)]) ; don't care what operator is. now make sure those two things are both type int
                                  ; Might want error checking here, can check the slides. week 6 type checking stuff.
                                  ; slide 64, 
                                  (cond [(and (types:IntType? t1) (types:IntType? t2)) (types:make-IntType)]
                                        ))]
           ; if not any of these things, guess need to return an error.
           [_ (error "Type Checking Error")]
           )])
    type-of-expr))