#lang typed/racket
(provide (all-defined-out))

(: symbol-downcase (-> Symbol Symbol))
(define (symbol-downcase sym)
   (string->symbol (string-downcase (symbol->string sym))))

(: eq*? (-> Any (Listof Any) Boolean))
(define (eq*? needle haystack)
   (not (not (member needle haystack))))

; javascript String#slice
(: slice (->* (String Integer) (Integer) String))
(define (slice str start [end (string-length str)])
   (: fix-idx (-> Integer Nonnegative-Integer))
   (define (fix-idx idx)
      (if (< idx 0)
         (max (+ idx (string-length str)) 0)
         idx))
   (substring str (fix-idx start) (fix-idx end)))

#|
(define-type Token (U (Syntaxof Any) EOF))
(: tok (-> String (Pairof Token String)))
(define (tok str)
   (define ip (open-input-string str))
   (cons (read-syntax "neu-internal" ip) (port->string ip)))
|#
(define-type Token Any)
(: tok (-> String (Pairof Token String)))
(define (tok str)
   (define ip (open-input-string str))
   (cons (read ip) (port->string ip)))

(: string-empty? (-> String Boolean))
(define (string-empty? s)
   (not (non-empty-string? s)))
