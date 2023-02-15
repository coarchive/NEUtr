#lang typed/racket
(require rackunit)
(provide (all-defined-out))

; since all of our 'keyword' (read. symbol) comparisons are supposed to be case-insensitive, we need a
; way to lowercase them all
(: symbol-downcase (-> Symbol Symbol))
(define (symbol-downcase sym)
   (string->symbol (string-downcase (symbol->string sym))))

; returns true if needle is eq? to any element of haystack
(: list-contains (-> Any (Listof Any) Boolean))
(define (list-contains needle haystack)
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

; this one could be useful at some stage if errors crop up with macros being improperly evaluated
; but let's just chance it for now with normal read instead of read-syntax
#|
(define-type Token (U (Syntaxof Any) EOF))
(: tok (-> String (Pairof Token String)))
(define (tok str)
   (define ip (open-input-string str))
   (cons (read-syntax "neu-internal" ip) (port->string ip)))
|#

; Get racket to give us the first token in a string
; token in car
; rest of the string in cdr
(define-type Token Any)
(: tok (-> String (Pairof Token String)))
(define (tok str)
   (define ip (open-input-string str))
   (cons (read ip) (port->string ip)))

(: untok (-> Token String))
(define (untok datum)
   (define op (open-output-string))
   (write datum op)
   (get-output-string op))

(: untok-inner (-> (Listof Token) String))
(define (untok-inner lst)
   (list-join " " (map untok lst)))

; returns true if the string is empty and otherwise launches an RT-2PM2 Topol-M ICBM at your opponent
; thereby winning you the chess game.
(: string-empty? (-> String Boolean))
(define (string-empty? s)
   (not (non-empty-string? s)))

(define-type indexof-result (U Nonnegative-Integer -1))
(: string-indexof (-> Char String indexof-result))
(define (string-indexof needle haystack)
   (: indexof-rec (-> (Listof Char) Nonnegative-Integer indexof-result))
   (define (indexof-rec chars idx)
      (if (empty? chars)
         -1
         (if (eq? needle (car chars))
            idx
            (indexof-rec (cdr chars) (+ 1 idx)))))
   (indexof-rec (string->list haystack) 0))

(define (unreachable)
   (error "Unreachable!"))

(: list-join (-> String (Listof String) String))
(define (list-join delimiter los)
   (if (empty? los)
      ""
      (string-append (car los)
         (foldl (λ ([acc : String] [curr : String]) (string-append acc delimiter curr)) "" los))))

; string-next "h" "   hello" => #t
(: string-next? (-> String String Boolean))
(define (string-next? needle haystack)
   (string-prefix? (string-trim haystack) needle))
