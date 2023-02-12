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

(: rw\lines (-> Lines Lines))
(define (rw\lines lines)
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

(: Lang? (-> String Boolean))
(define (Lang? str)
   (string-prefix? str "#lang"))

(: Comment? (-> String Boolean))
(define (Comment? str)
   (string-prefix? str ";"))

; <neu-comment>
(: rw\comment (-> Line Lines (Pairof Lines Lines)))
(define (rw\comment comment rest)
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
(: Template? (-> String (U #f TemplateInfo)))
(define (Template? text)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (and
      (symbol? first-token)
      (eq*? (symbol-downcase first-token) '(template template:))
      (cons text (cdr tok1))))

; variable : some type
; function : foo -> bar
; car: annotation target, cdr: after colon
(define-type AnnotationInfo (cons Symbol Line))
(: Annotation? (-> String (U #f AnnotationInfo)))
(define (Annotation? text)
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

(: A? (-> Symbol Boolean))
(define (A? sym) (eq*? (symbol-downcase sym) '(a an)))

; <neu-template>
; Template:\nfoo : bar
; Template: foo : bar
(: rw\template (-> TemplateInfo Lines (Pairof Lines Lines)))
(define (rw\template info rest)
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

(: prefix-template (-> Lines Lines))
(define (prefix-template lines)
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
   (define single-type : String (rw\neu-single-type (cdr info)))
   (define excess-type-info??? (cdr single-type))
   (if (non-empty-string? excess-type-info???)
      (error (string-append "Unexpected excess type info: " excess-type-info???))
      empty)
   (define annotation-type : String (car single-type))
   (return (string-append "(: " annotation-target " " annotation-type ")")))

(: rw\alias (-> AliasInfo Lines (Pairof Lines Lines)))
(define (rw\alias first rest)
   empty)

(define (string-next? [needle : String] [haystack : Line]) : Boolean
   (string-prefix? (string-trim haystack) needle))

(define (rw\neu-single-type [text : Line]) : (Pairof Line Line)
   (define tok1 (tok text))
   (define next-token (car tok1))
   (define (return (x : Line)) (cons x (cdr tok1)))
   (cond
      [(string-next? "{" text)
         (error "'{' not allowed here!")]
      [(list? next-token)
         (if (empty? next-token)
            '()
            (let ([internal-string (kot-inner next-token)])
               (return (rw\neu-type internal-string))))]
      [else
         (return (kot (rw\datum next-token)))]))

(define (rw\datum [datum : Any])
   (cond
      [(list? datum)
         (map rw\datum datum)]
      [(symbol? datum)
         (rw\typename datum)]
      [else
         datum]))

(define (rw\typename [name : Symbol]) : Symbol
   (cond
      [(eq? 'List-of name) 'Listof]
      [(eq? '??? name) 'Any]
      [else name]))

(define (rw\neu-type [text : Line]) : Line
   (define tok1 (tok text))
   (define first-token (car tok1))
   (define tok2 (tok (cdr tok1)))
   (define second-token (car tok2))
   )


(: append-bar (->* (String) (Positive-Integer) String))
(define (append-bar str [how-many 1])
    (apply string-append str (make-list how-many "bar")))
