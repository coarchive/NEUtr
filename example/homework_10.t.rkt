#lang typed/racket

(define-type Atom (U
  Number
  String
  Boolean
  Symbol))

; Template:
#;(: atom-temp (-> Atom Any))
#;(define (atom-temp a)
    (cond
      [(number? a) ...]
      [(string? a) ...]
      [(boolean? a) ...]
      [(symbol? a) ...]))

(define-type SExpr (U
  SExpr
  (Listof SExpr)))

#;(: sexpr-temp (-> SExpr Any))
#;(define (sexpr-temp s)
    (cond
      [(atom? s) (... (atom-temp s) ...)]
      [(list? s) (... (losexpr-temp s) ...)]))

; A ListOfSExpr is one of
; - '()
; - (cons SExpr ListOfSExpr)

(define-type ListOfSExpr (U '() (cons SExpr ListOfSExpr)))

(define-type ListOfBricks (U '() (cons 1 LOB)))
(define-type LOB ListOfBricks)

(: x LOB)
(define x '(1 1))

; id : {t} t -> t
