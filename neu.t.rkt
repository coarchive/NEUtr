#lang typed/racket

(define argv (current-command-line-arguments))
(define argc (vector-length argv))

(if (zero? argc)
  (error "neutr input_file.rkt [, input_file2.rkt...]")
  empty)

(define-type lines (Listof String))

(: rw\typename (-> String String))
(define (rw\typename name)
  (cond
    [(string=? "List-of" name) "Listof"]
    [(string=? "???" name) "Any"]
    [else name]))

(define (rw\type sntx rest))

(: rw\comment (-> String lines lines))
(define (rw\comment comment lines)
  empty)

(define (next str)
  (define ip (open-input-string str))
  (cons (read ip) (port->string ip)))
