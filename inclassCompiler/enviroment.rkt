#lang racket

; each file creates its own module. its more of a name space. own unit of code.
; so supply a bunch of definitinos in the module, unless you 'export' them in a way.

(provide (all-defined-out)) ; everything that will define, should be avaliable to other parts of the system.
; so makes writing tests really easy.

(require test-engine/racket-tests)


; create  a new empty enviroment
; basically a constructor, its a thunk (doesn't take any parameters
; (make-hash) is the constructor to make an empty hash table.
; so this function gives a list that has a literal hash inside it.
; will be extending it, and will be extending it with more hash tables
; the list will function as a stack.
(define (empty-env)
  (list (make-hash)))

;; extend an enviroment with a symbol and a value
; e.g (symbol, value)
; so current binding for symbol is, can ask later and it will give you value.
; add the symbol value pair to the hash table that is at the begiging of the stack.
(define (extend-env env sym val)
  (hash-set! (first env) sym val)  ; how to do a hash table setting. not functionally pure. it is mutability. imperative assignment operation.
  env)

; apply enviroment to a given symbol.
;; looking things up.
; return the associated value.
(define (apply-env env sym)
  (cond
    ; if give me an env that is absolutely empty, there is no associated value
    [(eq? env '()) #f] ; or throw an error, this should never happen unless someone is intentionally subverting the enviroment (empty-env has a hash in it.)
    [(hash-has-key? (first env) sym) (hash-ref (first env) sym)]
    [else (apply-env (rest env) sym)])) ; and so this is how scoping and shawdowing comes into play.

; only look for symbol in top most enviroment is another funciton but we skipping it



; WAnt these cause will have whole new sets of things. 
;push-scope
; push a new scope onto the enviroment.
(define (push-scope env)
  (cons (make-hash) env))

; pop-scope
; pop the first scope from the enviroment.
; this throws it away. so if want to know what the first hash table is, or scope, then going to have to get it yourself.
(define (pop-scope env)
  (rest env))

; how test this?
; (empty-env)
; (extend-env (empty-env) 'foo 5)
; (apply-env (extend-env (empty-env) 'foo 5) 'foo)

