#lang typed/racket
(require "util.rkt")
(provide (all-defined-out))

(: rw\file (-> String Void))
(define (rw\file filename)
   (define out (open-output-file (rw\filename filename)))
   (write (lines->string (rw\lines (file->lines filename))) out)
   (close-output-port out))

(: rw\filename (-> String String))
(define (rw\filename filename)
   (if (string-suffix? filename ".rkt")
      (string-append (slice filename 0 -4) ".t.rkt")
      filename))

(: lines->string (-> Lines String))
(define (lines->string lines)
   (foldl string-appendl "" lines))

(: string-appendl (-> String String String))
(define (string-appendl a b)
   (string-append* (list a b "\n")))

(define-type Line String)
(define-type Lines (Listof Line))

(: rw\lines (-> Lines Lines))
(define (rw\lines lines)
   (let ([head (car lines)] [tail (cdr lines)])
      (define comment-start? (string-indexof #\; head))
      (cond
         [(empty? lines)
            '()]
         [(Lang? head)
            (rw\lines tail)]
         [(ToplevelComment? head)
            (define comment-text (slice head 1))
            (define subcall (rw\toplevel-comment comment-text tail))
            (define rewritten (car subcall))
            (define rest (cdr subcall))
            (append rewritten (rw\lines rest))]
         [(<= 0 comment-start?)
            (define non-comment (slice head 0 comment-start?))
            (define comment-text (slice head (+ 1 comment-start?)))
            (define subcall (rw\inset-comment comment-text tail))
            (define rewritten (list (string-append non-comment subcall)))
            (define rest (cdr subcall))
            (append rewritten (rw\lines rest))]
         [else
            (cons head (rw\lines tail))])))

(: Lang? (-> String Boolean))
(define (Lang? str)
   (string-prefix? str "#lang"))

(: ToplevelComment? (-> String Boolean))
(define (ToplevelComment? str)
   (string-prefix? str ";"))

; <neu-toplevel-comment>
(: rw\toplevel-comment (-> Line Lines (Pairof Lines Lines)))
(define (rw\toplevel-comment comment rest)
   (cond
      [(Template? comment) (rw\template comment rest)]
      [(Alias? comment) (rw\alias text rest)]
      [else (cons (list comment) rest)]))

; <neu-inset-comment>
(: rw\inset-comment (-> Line Line))
(define (rw\inset-comment text rest)
   (if (Annotation? text)
      (rw\annotation text)
      text))

; Template:
; Template: fn : takes -> returns
(: Template? (-> String Boolean))
(define (Template? text)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (and
      (symbol? first-token)
      (eq*? (symbol-downcase x) '(template template:))))

; variable : some type
; function : foo -> bar
(: Annotation? (-> String Boolean))
(define (Annotation? text)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (define tok2 (tok (cdr tok1)))
   (define second-token (car tok2))
   (and
      (symbol? first-token)
      (eq? ': second-token)))

; A Car is a String
; An Color is one of\n; - x\n; - y\n; - z
(define (Alias? text)
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
      (eq? 'is (symbol-downcase third-token))))

(: A? (-> Symbol Boolean))
(define (A? sym) (eq*? (symbol-downcase sym) '(a an)))

; <neu-template>
; Template:\nfoo : bar
; Template: foo : bar
(: rw\template (-> String Lines (Pairof Lines Lines)))
(define (rw\template template-line rest)
   (define tok1 (tok template-line))
   (define after-template (cdr tok1))
   (define tok2 (tok after-template))
   (define should-be-eof (car tok2))
   (define effective-rest
      (if (eq? eof should-be-eof)
         rest
         (cons after-template rest)))
   (define annotation-head? (car effective-rest))
   (if (Annotation? annotation-head?)
      (begin
         (define annotation-res (rw\annotation annotation-head? (cdr effective-rest))))
         (define template-rest (prefix-template (car annotation-res)))
         (cons (prefix-template template-rest (cdr annotation-res)))
      (begin
         (define template-line-with-error
            (string-append template-line " <<< NEUtr: possible garbage at the end of template"))
         (cons (list template-line-with-error) rest))))

(: prefix-template (-> Lines Lines))
(define (prefix-template l)
   (map (λ (l) (string-append "#;" l))))

; <neu-annotation>
; foo : bar
; foo : bar -> baz
; you'll notice that rewriting the annotation doesn't actually use any lines except for the first one.
; we're gonna include the lines argument anyways to keep the rw\* functions looking similar
(: rw\annotation (-> String Lines (Pairof Lines Lines)))
(define (rw\annotation annotation rest)
   (define))

(define (rw\typename name)
   (cond
      [(string=? "List-of" name) "Listof"]
      [(string=? "???" name) "Any"]
      [else name]))

(: append-bar (->* (String) (Positive-Integer) String))
(define (append-bar str [how-many 1])
    (apply string-append str (make-list how-many "bar")))
