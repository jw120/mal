#lang typed/racket

(require "types.rkt")
(require/typed readline/readline
               [readline (-> String String)]
               [add-history (-> String Void)])

(define (READ [s : String]) : Mal
  1)

(define (EVAL [x : Mal]) : Mal
  x)

(define (PRINT [x : Mal]) : String
  "Q")

(define (rep [s : String]) : String
  (PRINT (EVAL (READ s))))

(define (repl) : Void
  (define user-input : String (readline "user> "))
  (cond [(eq? user-input eof)
         (displayln)] ; On Ctrl-D, clean up by printing a newline
        [(non-empty-string? user-input)
         (add-history user-input)
         (displayln (rep user-input))
         (repl)]
        [else
         (repl)]))

