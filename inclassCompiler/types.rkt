#lang racket

; this is some of the type stuff.

(provide (all-defined-out))

; here are the types for the NI language
; so,
; a base type called the nitype which you never actually build one of these things specifically. all types are derived from this.
; specify what the actual type will be.
; check that properties of actual and typename arctually are.
; actual is for like arrays and records the actual type (like inside type i guess)
(struct NiType ([actual #:mutable]) #:transparent
  #:guard (λ (actual typename) ; this guard is inherited.
            (if (eq? typename 'NiType)
                (error "Cannot Instantiate NiType directly.")
                (if (or (eq? actual '())
                        (NiType? actual))
                    (values actual) ; what want to return. create values from that actual thing.
                    (raise-arguments-error typename "Can only instantiate with NiTypes or '()" "actual: " actual)))))

; what an actual type looks like.
; string type
; if want to build a struture of stringtype, it is a nitype with actual type of emmpty.
; could end there, and then
; needs a parameter, cause (StringType) causes an error
; (StringType '()) works. <- need to define this way like every time.
; the actual type is used for constructed types, could have a string type, then what is the thing that makes up the string.
; so could give it an empty actual type. but when try to define a list type, want to say its a list of strings or a list of whatever
; so for strings, will alsways give an empty list. because string is a literalish type in our language. 
;(struct StringType NiType () #:transparent)
(struct StringType NiType () #:transparent
  ; add something that will let us control how it gets printed out. so debuging is easier. 
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode)
                                  (write-string "t:str" port)))])

; DID I MISS A VOID TYPE?????
(struct VoidType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode)
                                  (write-string "t:void" port)))])
(struct IntType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode)
                                  (write-string "t:int" port)))])
(struct BoolType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode)
                                  (write-string "t:bool" port)))])
(struct PengType NiType () #:transparent
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode)
                                  (write-string "t:peng" port)))])

; a name type
; now using the actual parameter.
(struct NameType NiType (name) #:transparent
  #:methods gen:custom-write [(define write-proc
                                (λ (ty port mode) ; ty is type
                                  ; with these objects, can control when write it, or display it.
                                  ; write is intended for a port(like a network stream)
                                  ; display is for it to be human readable like for the screen.
                                  ; this is controlling whewre the thing iwll get prtininted to
                                  (let ([theprinter (case mode
                                                      [(#t) write]
                                                      [(#f) display]
                                                      [else (λ (p port) (print p port mode))])])
                                    (write-string "<name: " port)
                                    (theprinter (NiType-actual ty) port) ; if the name is actually of int type, if the name is a funciton, then we will see function type
                                    (write-string ">" port))))]
  ; need to make sure it is actually a symbol of something. if doing in java, there aren't things like this. arent guards.
  #:guard (λ (actual name tyname)
            (cond
              ; not a symbol, thats bad. something like 5
              [(and (not (symbol? name))
                    (raise-arguments-error tyname "NameType name must be a symbol"
                                           "actual" actual
                                           "name" name))]
              [else (values actual name)])))

; (NameType '(IntType))
; get an error
; (NameType 'foo 'IntType)
; error otherway around
; (NameType (IntType '()) 'foo) <- WORKS THATS THE USAGE FOR NAMETYPE!!!!


; arrayType
(struct ArrayType NiType (name element-type
                               [label #:mutable #:auto]) #:transparent
  #:auto-value #f
  #:methods gen:custom-write [(define write-proc (λ (ty port mode)
                                                   (let
                                                       ([theprinter (case mode
                                                                      [(#t) write]
                                                                      [(#f) display]
                                                                      [else (λ (p port) (print p port mode))])])
                                                     (display "t:array[ " port)
                                                     (theprinter (ArrayType-element-type ty) port) ;print actual type of elements
                                                     (display " ]" port))))])

; build an array type.
; expects 3 arguments
; (ArrayType (IntType '()) 'alist 'foo)
; both of these work. but the 'foo works cause there isn't a guard.
; > (ArrayType (IntType '()) 'alist (IntType '()))


;;; Functions ofr Building Types (like constructors for base types
; these two have no actual type besides its own base type
; these all base types.
(define (make-IntType)
  (IntType '()))
(define (make-StringType)
  (StringType '()))
(define (make-BoolType)
  (BoolType '()))

(define (make-PengType)
  (PengType '()))

; Need this one
(define (make-VoidType)
  (VoidType '()))





; MISSING
; REcord types
;     NameTypePair (for the fields of the record


;;; Valuesfor Ni
;;; VarValue (for the named Variables)
;;; FunValue (for the named functions)



(struct foo (x y) #:transparent)
; (foo 5 10) -> works
