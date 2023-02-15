#lang typed/racket
(require "util.rkt")
(provide (all-defined-out))

(define (rw\file [filename : String]) : Void
   (define out (open-output-file (rw\filename filename)))
   (write (lines->string (rw\lines (file->lines filename))) out)
   (close-output-port out))

(define (rw\filename [filename : String]) : String
   (if (string-suffix? filename ".rkt")
      (string-append (slice filename 0 -4) ".t.rkt")
      filename))

(define (lines->string [lines : Lines]) : String
   (foldl string-appendl "" lines))

(define (string-appendl [a : String] [b : String]) : String
   (string-append* (list a b "\n")))

(define-type Line String)
(define-type Lines (Listof Line))
(define-type TypeString String)
(define-type TypeStrings (Listof TypeString))

(define (rw\lines [lines : Lines]) : Lines
   (let ([head (car lines)] [tail (cdr lines)])
      (cond
         [(empty? lines)
            '()]
         [(Lang? head)
            (rw\lines tail)]
         [(Comment? head)
            (define comment-text (slice head 1))
            (define subcall (rw\comment comment-text tail))
            (define rewritten (car subcall))
            (define rest (cdr subcall))
            (append rewritten (rw\lines rest))]
         [else
            (cons head (rw\lines tail))])))

(define (Lang? [str : String]) : Boolean
   (string-prefix? str "#lang"))

(define (Comment? [str : String]) : Boolean
   (string-prefix? str ";"))

; <neu-comment>
(define (rw\comment [comment : Line] [rest : Lines]) : (Pairof Lines Lines)
   (define template-info? (Template? comment))
   (define annotation-info? (Annotation? comment))
   (define alias-info? (Alias? comment))
   (cond
      [template-info? (rw\template template-info? rest)]
      [annotation-info? (rw\annotation annotation-info? rest)]
      [alias-info? (rw\alias alias-info? rest)]
      [else (cons (list comment) rest)]))


; Template:
; Template: fn : takes -> returns
; car: full template line, cdr: after template
(define-type TemplateInfo (cons Line Line))
(define (Template? [text : String]) : (U #f TemplateInfo)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (and
      (symbol? first-token)
      (list-contains (symbol-downcase first-token) '(template template:))
      (cons text (cdr tok1))))

; variable : some type
; function : foo -> bar
; car: annotation target, cdr: after colon
(define-type AnnotationInfo (cons Symbol Line))
(define (Annotation? [text : String]) : (U #f AnnotationInfo)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (define tok2 (tok (cdr tok1)))
   (define second-token (car tok2))
   (and
      (symbol? first-token)
      (eq? ': second-token)
      (cons first-token (cdr tok2))))

; A Car is a String
; An Color is one of\n; - x\n; - y\n; - z
(define-type AliasInfo (cons Symbol Line))
(define (Alias? [text : String]) : (U #f AliasInfo)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (define tok2 (tok (cdr tok1)))
   (define second-token (car tok2))
   (define tok3 (tok (cdr tok1)))
   (define third-token (car tok2))
   (and
      (symbol? first-token)
      (A? first-token)
      (symbol? second-token)
      (symbol? third-token)
      (eq? 'is (symbol-downcase third-token))
      (cons second-token (cdr tok3))))

(define (A? [sym : Symbol]) : Boolean
   (list-contains (symbol-downcase sym) '(a an)))

; <neu-template>
; Template:\nfoo : bar
; Template: foo : bar
(define (rw\template [info : TemplateInfo] [rest : Lines]) : (Pairof Lines Lines)
   (define fulltext : String (car info))
   (define after-template : String (cdr info))
   (define tok1 (tok after-template))
   (define should-be-eof (car tok1))
   (: effective-rest Lines)
   (define effective-rest
      (if (eq? eof should-be-eof)
         rest
         (cons after-template rest)))
   (define annotation-head? (car effective-rest))
   (define annotation-info? (Annotation? annotation-head?))
   (if annotation-info?
      (let ()
         (define annotation-res (rw\annotation annotation-info? (cdr effective-rest)))
         (define template-rest (prefix-template (car annotation-res)))
         (cons (prefix-template template-rest) (cdr annotation-res)))
      (let ()
         (define template-line-with-error
            (string-append fulltext "| NEUtr: garbage at end of template |" ))
         (cons (list template-line-with-error) rest))))

(define (prefix-template [lines : Lines]) : Lines
   (map (λ ([l : String]) (string-append "#;" l)) lines))

; <neu-annotation>
; foo : bar
; foo : bar -> baz
; you'll notice that rewriting the annotation doesn't actually use any lines except for the first one.
; we're gonna include the lines argument anyways to keep the rw\* functions looking similar
(define (rw\annotation [info : AnnotationInfo] [rest : Lines]) : (Pairof Lines Lines)
   (define (return [x : String]) : (Pairof Lines Lines)
      (cons (list x) rest))
   (define annotation-target : String (symbol->string (car info)))
   (define after-colon (cdr info))
   (define annotation-type : String (rw\neu-annotation-type after-colon))
   (return (string-append "(: " annotation-target " " annotation-type ")")))


(define (Function? [text : Line]) : Boolean
   (define all-tokens (car (tok (string-append "(" text ")"))))
   (cond
      [(list? all-tokens)
         (list-contains '-> all-tokens)]
      [else
         (error "I FUCKING KNOW IT'S A LIST YOU PIECE OF GARBAGE")]))


; returns something like "(: x Number)"
(define (rw\neu-annotation-type [text : Line]) : TypeString
   (cond
      [(Function? text)
         (rw\neu-function text)]
      [else
         (rw\neu-type text)]))

(define (rw\neu-function [text : Line]) : TypeString
   (cond
      [(string-next? "{" text)
         (define tok1 (tok text))
         (define type-params (car tok1))
         (define after-type-params (cdr tok1))
         (define type-param-string (untok type-params))
         (define func-string (rw\neu-actual-function after-type-params))
         (string-append "(All " type-param-string " " func-string ")")]
      [else
         (rw\neu-actual-function text)]))

; returns something like "(-> Integer String)"
(define (rw\neu-actual-function [text : Line]) : Line
   (define (typestring-rec [s : String]) : String
      (define tok1 (tok s))
      (define next-token (car tok1))
      (define rest (cdr tok1))
      (cond
         [(string-next? "{" s)
            (error "'{' not allowed here!")]
         [(eq? eof next-token)
            ""]
         [(eq? '-> next-token)
            (typestring-rec rest)]
         [else
            (define rewritten (rw\neu-type next-token))
            (string-append " " rewritten (typestring-rec rest))]))
   (string-append "(->" (typestring-rec text) ")"))

(define (rw\neu-type [token : Any]) : String
   (cond
      [(eq? '() token)
         "'()"]
      [(list? token)
         ; this doesn't actually work lol because {} will be converted into ()
         ; let's just pretend it works. sure hope nobody does any generic functions in neu-types!
         (rw\neu-type-inlist (untok-inner token))]
      [else
         (untok (rw\atom token))]))

; returns something in a list like (-> Foo Bar) or (Listof Blah)
(define (rw\neu-type-inlist [s : String]) : String
   (cond
      [(Function? s)
         (rw\neu-function s)]
      [else
         (rw\neu-generic-type s)]))

(define (rw\neu-generic-type [s : String]) : String
   (define tok1 (tok s))
   (define typename (car tok1))
   (define after-typename (cdr tok1))
   (define (typestring-rec [s : String]) : String
      (define tok1 (tok s))
      (define next-token (car tok1))
      (define rest (cdr tok1))
      (cond
         [(eq? eof next-token)
            ""]
         [else
            (define rewritten (rw\neu-type next-token))
            (string-append " " rewritten (typestring-rec rest))]))
   (cond
      [(symbol? typename)
         (define typestring (typestring-rec after-typename))
         (string-append "(" (symbol->string typename) " " typestring ")")]
      [else
         (error "The target of a generic type must be a symbol!")]))

(define (rw\atom [datum : Any])
   (cond
      [(symbol? datum)
         (rw\typename datum)]
      [else
         datum]))

(define (rw\typename [name : Symbol]) : Symbol
   (cond
      [(eq? 'List-of name) 'Listof]
      [(eq? '??? name) 'Any]
      [else name]))

(define (rw\alias [info : AliasInfo]) : (Pairof Lines Lines)
   (define symbol ))
