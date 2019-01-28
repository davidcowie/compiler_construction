#lang racket

(require parser-tools/lex (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)
(require racket/cmdline)
(provide (all-defined-out))

;(current-command-line-arguments)
; https://rosettacode.org/wiki/Parse_command-line_arguments#Racket
(define filename "")
(command-line
 #:args (file) (set! filename file))

;(print filename)

; defining tokens
(define-tokens value-tokens (NUM ID STRING))
(define-empty-tokens paren-types (LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE))
(define-empty-tokens operators (ADD MULT DIV SUB DOT))
(define-empty-tokens punctuation (COMMA COLON SEMI))
(define-empty-tokens comparators (EQ NE LT GT LE GE))
(define-empty-tokens boolops (BOOLOR BOOLAND))

(define-tokens test-tokens (TEST TEST2))

(define-empty-tokens keywords (AND ARRAY AS BREAK DEFINE DO ELSE END IF IN IS
 JUNIPER KIND LET NEEWOM NI NOW OF PENG THEN
 TO WHILE WITH))

(define-empty-tokens endoffile (EOF))

; and thats all given.

(define lexer
  (lexer-src-pos
   
   [#\+ (token-ADD)]
   [#\* (token-MULT)]
   [#\/ (token-DIV)]
   [#\- (token-SUB)]
   [#\. (token-DOT)] ; not fully sure this was the dot.
   [#\, (token-COMMA)]
   [#\: (token-COLON)]
   [#\; (token-SEMI)]
   ; Appears the order for LE and just less than string doesn't matter. it gets it for GE and LE
   ["<=" (token-LE)]
   [#\(                                             (token-LPAREN)]
   [#\{                                             (token-LBRACE)]
   [#\[                                             (token-LBRACKET)]
   [#\<                                             (token-LT)]
   [#\)                                             (token-RPAREN)]
   [#\}                                             (token-RBRACE)]
   [#\]                                             (token-RBRACKET)]
   [#\>                                             (token-GT)]
   [">=" (token-GE)]
   ["<>" (token-NE)] ; NOT SURE THIS IS THE NOT EQUAL!!!
   ; need GE and LE and NE
   [#\= (token-EQ)]
   [#\& (token-BOOLAND)]
   [#\| (token-BOOLOR)]
   ["and" (token-AND)]
   ["array" (token-ARRAY)]
   ["as" (token-AS)]
   ["break" (token-BREAK)]
   ["define" (token-DEFINE)]
   ["do" (token-DO)]
   ["else" (token-ELSE)]
   ["end" (token-END)]
   ["if" (token-IF)]
   ["in" (token-IN)]
   [(:or "is" "IS") (token-IS)]
   ["juniper" (token-JUNIPER)]
   ["kind" (token-KIND)]
   ["let" (token-LET)]
   ["neewom" (token-NEEWOM)]
   ["ni" (token-NI)]
   ["now" (token-NOW)]
   ["of" (token-OF)]
   ["peng" (token-PENG)]
   
   ;[#\space (lexer input-port)] ; not what i want
   [whitespace (return-without-pos (lexer input-port))]
   [#\0 (token-NUM 0)]
   ;[(:: (char-range #\1 #\9) (:* (char-range #\0 #\9))) (token-NUM (string->number lexeme))]
   ; I think should convert a number, to a number right away rather than leave it as a string.
   [(:: (char-range #\1 #\9) (:* (char-range #\0 #\9))) (token-NUM lexeme)]
   ; NEED ID and STRING Still!!!
   ; removing the quotes from the thing
   ;[(:: "\"" (complement (:: any-string "\"" any-string)) "\"") (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   ; STRING version 2. This is based off of the extra description, I think both versions seem to work.
   ;[(:: "\"" (:or (:: "\\" any-char) (complement (:or "\"" "\\"))) "\"") (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   ; This one now seems to work for the strings as they are wanted.
   [(:: #\" (:* (:or (:: #\\ any-char)#|(:or #\" #\\ #\r #\t #\n))|# (:~ (:or #\" #\\)))) #\") (token-STRING lexeme)]
   ;[(:: #\" #\\ #\" #\") (token-TEST2 lexeme)]
   ; ID
   ; before had this, if there wasn't a space between is55, it would return two tokens, an is token and a num token
   ; but now it gets rid of that issue and it will be an ID. which I believe is incorrect. But i guess this solved the white space issue...
   ; so is55 works correctly, but if type 8hi, that gives me a num roken and an id token...
   ; because don't just want everything to need to be separated by whitespace, cause like parenthesis in his function example...
   ; but should 8hi be correct, or should that be a syntax error....
   ; only certain tokens want to have a space after, and i think numbers might be the only ones.. except if its a parenthesis...
   ; so that makes it confusing...
   [(:: (:or upper-case lower-case) (:* (:or upper-case lower-case numeric "_" "-")) (:* "'")) (token-ID lexeme)]
   ; NEED END OF LINE!!!
   ; COMMENTS
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") (return-without-pos (lexer input-port))] ; how handle comments
   ; need inline comments //
   ; NOT FINISHED!!!! - THINKING DIFFICULT TO TEST!!!
   ; not sure why it isn't ending with a newline... with the complement
   ;[(:: "//" (:* (complement (:or "\n" "newline"))) "newline") (begin (print lexeme) (return-without-pos (lexer input-port)))] ; might need to check and end of line or something.
   ; this second one worked. I guess using the char for new line was different that that string...
   ; or maybe char-complement works different than compliment.
   [(:: "//" (:* (char-complement #\newline))) (return-without-pos (lexer input-port))] ; might need to check and end of line or something.
   [(eof) (token-EOF)]

   [any-char (error "ERROR")]
   ))



(define (get-tokenizer in)
  (λ () (lexer in)))

(define (lex in)
  (let ([tokenize (get-tokenizer in)])
    (define (lexfun)
      (let ([tok (tokenize)])
        
        (cond
          [(eq? (position-token-token tok) (token-EOF)) null]
          [else (cons (position-token-token tok) (lexfun))])))
    (lexfun)))

(define (lexstr str)
  (lex (open-input-string str)))

(define (lexfile filename)
  (lex (open-input-file filename)))

(lexfile filename)