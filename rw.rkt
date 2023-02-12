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

(define-type Lines (Listof String))
(: rw\lines (-> Lines Lines))
(define (rw\lines lines)
   (let ([head (car lines)] [tail (cdr lines)])
      (cond
         [(empty? lines)
            '()]
         [(comment? head)
            (define subcall (rw\comment head tail))
            (: rewritten Lines)
            (define rewritten (car subcall))
            (: rest Lines)
            (define rest (cdr subcall))
            (append rewritten (rw\lines rest))]
         [else
            (cons head (rw\lines tail))])))

(: comment? (-> String Boolean))
(define (comment? str)
   (string-prefix? str ";"))

(: rw\comment (-> String Lines (Pairof Lines Lines)))
(define (rw\comment comment rest)
   (define text (slice comment 1))
   (cond
      [(Template? text) (rw\template text rest)]
      [(Annotation? text) (rw\function text rest)]
      [(Alias? text) (rw\alias text rest)]
      [else (cons (list comment) rest)]))

; Template:
; Template: fn : takes -> returns
(: Template? (-> String Boolean))
(define (Template? text)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (and
      (symbol? first-token)
      (eq*? (symbol-downcase x) '(template template:))))

(: rw\template (-> String Lines (Pairof Lines Lines)))
(define (rw\template should-be-empty-string lines)
   empty)

(: Annotation? (-> String Boolean))
(define (Annotation? text)
   (define tok1 (tok text))
   (define first-token (car tok1))
   (define tok2 (tok (cdr tok1)))
   (define second-token (car tok2))
   (and
      (symbol? first-token)
      (eq? ': second-token)))

(define (Alias? ))

(: A? (-> Symbol Boolean))
(define (A? sym) (eq*? (symbol-downcase sym) '(a an)))

(define (rw\typename name)
   (cond
      [(string=? "List-of" name) "Listof"]
      [(string=? "???" name) "Any"]
      [else name]))

(: append-bar (->* (String) (Positive-Integer) String))
(define (append-bar str [how-many 1])
    (apply string-append str (make-list how-many "bar")))
