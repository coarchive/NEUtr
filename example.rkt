#lang racket

; A [Some t] is (list t)
; A None is '()

; An [Option t] is one of
; - Some t
; - None

(define sp (open-input-string "foo bar"))
(read sp)
(print (read sp))
