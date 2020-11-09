#lang typed/racket

(require "printer.rkt" "types.rkt")
(require/typed readline/readline
               [readline (-> String (U String EOF))]
               [add-history (-> String Void)])

(define (READ [s : String]) : Mal
  1)

(define (EVAL [x : Mal]) : Mal
  x)

(define (PRINT [x : Mal]) : String
  (pr_str x true))

(define (rep [s : String]) : String
  (PRINT (EVAL (READ s))))

(define (repl) : Void
  (define user-input : (U String EOF) (readline "user> "))
  (cond [(eq? user-input eof)
         (displayln "")] ; On Ctrl-D, clean up by printing a newline
        [(non-empty-string? user-input)
         (add-history user-input)
         (displayln (rep user-input))
         (repl)]
        [else
         (repl)]))

(repl)