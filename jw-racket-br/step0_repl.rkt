#lang racket/base

(require readline/readline)

(define (repl)
  (define input-string (readline "user> "))
  (cond
    [(eq? input-string eof)
     (displayln "")] ; On Ctrl-D, clean up by printing a newline
    [else
       (displayln input-string)
       (repl)]))

(repl)
