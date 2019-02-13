#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(provide (all-defined-out))

; https://docs.racket-lang.org/parser-tools/Lexers.html#%28part._.Creating_a_.Lexer%29

;(define a 5)

(define-tokens numeric-values (NUM))
; could do ADDITION SUBTRACTION
(define-empty-tokens math-operations (ADDITION SUBTRACTION MULTIPLACATION DIVISION))
(define-empty-tokens end-of-file (EOF))
(define-empty-tokens parenthesis (LPAREN RPAREN))

#|
(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "-")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))
|#


(define lexer
  (lexer-src-pos
   [#\+ (token-ADDITION)]
   [#\- (token-SUBTRACTION)]
   [#\* (token-MULTIPLACATION)]
   [#\/ (token-DIVISION)]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [#\0 (token-NUM 0)]
   [(:: (char-range #\1 #\9) (:*(char-range #\0 #\9))) (token-NUM (string->number lexeme))]
   [whitespace (return-without-pos (lexer input-port))]
   [(:: "#lang" (:+ (union #\space #\tab)) (union "dbn" "dbn-lang")) (return-without-pos (lexer input-port))]
   [(eof) (token-EOF)]))

(define (get-tokenizer in)
  (λ () (lexer in)))

(define (lexstr str)
  (let ([input (open-input-string str)])
    (λ () (lexer input))))

(define (get-all-tokens in)
  (let ([tokenize (lexstr in)])
    (define (lexfun)
      (let ([tok (tokenize)])
        (cond
          [(eq? (position-token-token tok) (token-EOF)) null]
          [else (cons (position-token-token tok) (lexfun))])))
    (lexfun)))

(define (token-seq toks)
  (λ () (first toks)))

(define simple-parser
  (parser
   (start exp)
   (end EOF)
   (error void)
   (tokens numeric-values math-operations end-of-file parenthesis)
   (grammar
    (exp ((NUM) $1)
         ((exp ADDITION exp) (+ $1 $3))
         ((exp SUBTRACTION exp) (- $1 $3))))))

(define (parse in)
  (port-count-lines! in)
  (simple-parser (get-tokenizer in)))

(define (parse-str str)
  (let ([in (open-input-string str)])
    (parse in)))
    
    
   


