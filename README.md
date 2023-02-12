# NEUtr

*Convert NEU type comments into typed racquet*

In our latest batch of avoidance-based-programming (new paradigm invented by yours truly), I make another lisp meme. This language is truly one of the most frustrating languages that I've used. Perhaps even more frustrating than Common Lisp. That just makes the payoff all the more worth it, though. "No pain, no pain" as the kids say.

## example

```lisp
#lang racket

; A [Some t] is (list t)
; A None is '()

; An [Option t] is one of
; - Some t
; - None

(define sp (open-input-string "foo bar"))
(read sp)
(print (read sp))
```
