#lang racket

(define x 5)

; from the in class lab:
; expression ::= digit | '(' experssion operator expression ')'
; operator ::= '+' | '*'
; digit ::= '0' | '1' | '2' | .... | '9'

; thats the language setup.
; write some kind of a lexer that will take in strings

; build a structure that represents a token in this little language.
; two ways to do this. we are doing this byy hand. it will have two things,, what kind of token( a type) and then the actual value.
; has two parts, type and repr
; if stop, can build tokens
; (token 'op "+")
; transparent, can print the things that are instide the structure.
; would like to have some kind of checking for the structures - like a representation is a valid form of the type.
; add in a guard to check
; defining guard function, give type, name, representation, and the name of the struct.
#|
(struct token (type repr) #:transparent
  #:guard (lambda (typ repr struct-name)
            (if (not (is-token-type? typ))
                (error "Unknowm type: " typ)
                (if (and (not (eq? eof repr))
                         (not (char? repr))) ; not an end of file, and not a character, then problem
                    (error "Expected char or eof, got: " repr) ; if the if statment is true
                    (values typ repr))))) ; what valeus going to return.
; so if building byy hand, its good to build these guards.
|#
; this is a new version!!!
(struct token (type repr) #:transparent
  #:guard (lambda (typ repr struct-name)
            (if (not (is-token-type? typ))
                (error "Unknowm type: " typ)
                (if (and (not (eq? eof repr))
                         (not (char? repr))
                         (not (string? repr))) ; not an end of file, and not a character, then problem
                    (error "Expected char or eof, got: " repr) ; if the if statment is true
                    (values typ repr))))) ; what valeus going to return.
; so if building byy hand, its good to build these guards.

(define (is-token-type? typ)
  ; if this thing, n the first part of the toekem, want to know if one of these things
  (cond ([or (eq? typ 'op)
             (eq? typ 'lparen)
             (eq? typ 'rparen)
             (eq? typ 'digit)
             (eq? typ 'eof)] true)
         (else false)))

(define (get-next-token input-port)
  (let ([c (read-char input-port)])
    (cond ([eq? c #\) ] (token 'rparen c))
          ([eq? c #\( ] (token 'lparen c))
          ([eq? c #\+ ] (token 'op c))
          ([eq? c #\* ] (token 'op c))
          ([eq? c eof ] (token 'eof c))
          ([or (eq? c #\0)
               (eq? c #\1)
               (eq? c #\2)
               (eq? c #\3)
               (eq? c #\4)
               (eq? c #\5)
               (eq? c #\6)
               (eq? c #\7)
               (eq? c #\8)
               (eq? c #\9)] (token 'digit c))
          (else (raise-syntax-error #f
                                    (string-append "Unexpected Syntax: " (string c))))
          )))

(define (get-next-token2 input-port)
  (let ([c (read-char input-port)])
    (cond ([eq? c #\) ] (token 'rparen c))
          ([eq? c #\( ] (token 'lparen c))
          ([eq? c #\+ ] (token 'op c))
          ([eq? c #\* ] (token 'op c))
          ([eq? c eof ] (token 'eof c))
          ([or (eq? c #\0)
               (eq? c #\1)
               (eq? c #\2)
               (eq? c #\3)
               (eq? c #\4)
               (eq? c #\5)
               (eq? c #\6)
               (eq? c #\7)
               (eq? c #\8)
               (eq? c #\9)] (let ([c1 (read-char input-port)]) (begin (print c1) (print c) (token 'digit c))))
          (else (raise-syntax-error #f
                                    (string-append "Unexpected Syntax: " (string c))))
          )))

(define (lexstr str)
  (let ([input (open-input-string str)])
    (λ () (get-next-token input))))
; so this returns a thunk, a procedure, with the data.

; helper function
(define (get-all-tokens lex)
  ; need a let* because using tok to define typ
  (let* ([tok (lex)]
        [typ (token-type tok)]) ; how to extract things from structures.
    (if (eq? typ 'eof)
        '() ; empty list
        (cons tok (get-all-tokens lex)))))

; like to have a function, maybe called parse, and it can figure out if the thing is valid or not.
; like to pass in lexstr on something
; (parse (lextstr "((1+2)*3)"))
; somehow look at the tokens, and know that if see a plus, can only appear at certain places.
; and then build up a tree structure perhaps...
; this is the hardest part of this.
; out of the example, want,
; numbers are lower down

;(define (parse-digit lex)


; if have a digit, then done. its a terminal guy.
(define (parse lex)
  (let* ([tok (lex)]
        [typ (token-type tok)]
        [val (token-repr tok)])
    (cond ([eq? typ eof] '())
          ([eq? typ 'digit] (string->number (string val))) ; digit is terminal, so just return the list with that 
          ; want to parse the next thing, has to be an expr, then the next thing has to be an op
          ([eq? typ 'lparen] (parse-expr-op-expr lex))
          (else (error "Invalid expression: " val)))))

; result that want to return?
; every possiblity gets its own function. for recursive form parsing.
(define (parse-expr-op-expr lex)
  (let* ([e1 (parse lex)]
         [op (parse-operator lex)]
         [e2 (parse lex)]
         [tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (if (eq? typ 'rparen)
        (list op e1 e2)
        (error "Syntax Error: Missing closing parenthesis."))))

(define (parse-operator lex)
  (let* ([tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    ; the guard handles that it will be + and *
    (if (eq? typ 'op)
        val
        (error "Syntax error: Invalid operation : " val))))

; now to actually evaluate the thing.
(define (eval ast)
  (cond ([number? ast] ast) ; becausei f just give a digit, just return a digit.
        ([list? ast] (if (eq? (first ast) #\+)
                         (+ (eval (first (rest ast))) (eval (first (reverse ast))))
                         (* (eval (first (rest ast))) (eval (first (rest (rest ast)))))))))


(define (eval-str str)
  (let ([lexer (lexstr str)])
    (eval (parse lexer))))


(struct ast-node (val) #:transparent)
(struct ast-expr-node (operator left-child right-child) #:transparent)
(define (parse2 lex)
  (let* ([tok (lex)]
         [typ (token-type tok)]
         [val (token-repr tok)])
    (cond ([eq? typ eof] '())
          ([eq? typ 'digit] (ast-node (string->number (string val))))
          ([eq? typ 'lparen] (parse-exp-op-exp2 lex))
          (else (error "Invalid")))))
                                      

(define (parse-exp-op-exp2 lex)
  (let* ([ep1 (parse2 lex)]
         [op (parse-op2 lex)]
         [ep2 (parse2 lex)]
         [tok (lex)]
         [type (token-type tok)])
    (if (eq? type 'rparen)
        ;(list op ep1 ep2)
        (ast-expr-node op ep1 ep2)
        (error "Missing Closing Prenthesis. 2"))))

(define (parse-op2 lex)
  (let* ([tok (lex)]
         [type (token-type tok)]
         [val (token-repr tok)])
    (if (eq? type 'op)
        val
        (error "Unknown operator: " val))))

(define (eval2 ast)
  (match ast
    ([ast-node v] v)
    ([ast-expr-node op exp1 exp2] (if (eq? op #\+)
                                      (+ (eval2 exp1) (eval2 exp2))
                                      (* (eval2 exp1) (eval2 exp2))))))

(define (eval-str2 str)
  (let ([lexed (lexstr str)])
    (eval2 (parse2 lexed))))

; for multi digit numbers, instead of accepting those as initial tokens, hard because a stream.
; could do it once have list of tokens, then combine two adjacent digits.
(define (lexstrTEST str)
  (let ([input (open-input-string str)])
    (begin (print input) (λ () (get-next-token2 input)))))

; combine tokens
(define (combine-tokens toks)
  (if (null? toks)
      '()
      (let* ([tok (first toks)]
             [typ (token-type tok)]
             [val (token-repr tok)])
        (if (eq? typ 'digit)
            (cons (parse-digit-tokens toks) (combine-tokens (rest toks))) ; rest won't work unless remove from the toks list
            (cons tok (combine-tokens (rest toks)))))))
#|
(define (parse-digit-tokens tokss)
  (define (f toks)
    (if (null? toks)
        '()
        (let* ([tok (first toks)]
               [typ (token-type tok)]
               [val (token-repr tok)])
          (if (eq? typ 'digit)
              (cons val (f (rest toks)))
              (list toks)))))
  (let ([wholeThing (f tokss)])
    (begin (print wholeThing) (cons (token 'digit (list->string (first wholeThing))) (rest wholeThing)))))
  ;toks)))
|#
(define (parse-digit-tokens tokss)
  (define (f toks)
    (if (null? toks)
        '()
        (let* ([tok (first toks)]
               [typ (token-type tok)]
               [val (token-repr tok)])
          (if (eq? typ 'digit)
              (cons val (f (rest toks)))
              '()))))
  (define (f2 toks)
    (if (null? toks)
        '()
        (let* ([tok (first toks)]
               [typ (token-type tok)]
               [val (token-repr tok)])
          (if (eq? typ 'digit)
              (f2 (rest toks))
              toks))))
  (let ([wholeThing (f tokss)]
        [otherThing (f2 tokss)])
    ;(begin (print wholeThing) (print otherThing) (cons (token 'digit (list->string wholeThing)) otherThing))))
    (cons (token 'digit (list->string wholeThing)) otherThing)))