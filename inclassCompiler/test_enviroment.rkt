#lang racket

; enviroment tests
(require "enviroment.rkt" test-engine/racket-tests)

; hash-copy says take that and build a copy of it. see if necessary
; a back quote, like normal but allowed to add commas inside things.
; so the list will have the actual result of that function call.
;'(+ 5 3) --> gets the literal text.
;`(,(+ 5 3))
(check-expect (empty-env) `(, (hash-copy #hash())))

(check-expect (extend-env (empty-env) 'x 5) `(, (hash-copy #hash((x . 5)))))
; these have same represenetation printed out, but they are not the same thing.
;(check-expect (extend-env (empty-env) 'x 5) `(, #hash((x . 5))))

(check-expect (apply-env (extend-env (empty-env) 'y 7) 'y) 7)
(check-expect (apply-env (empty-env) 'x) #f)

; test pusching and popping
; should have two empty hashes.
; the comma is indicitive of evaluate
(check-expect (push-scope (empty-env)) `(,(make-hash) ,(make-hash)))

(check-expect (pop-scope (push-scope (empty-env))) `(, (make-hash)))

(test)
