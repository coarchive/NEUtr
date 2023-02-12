#lang typed/racket

(define argv (current-command-line-arguments))
(define argc (vector-length argv))

(if (zero? argc)
   (error "neutr input_file.rkt [, input_file2.rkt...]")
   empty)
