#lang typed/racket

(require/typed readline/readline
               [readline (-> String String)]
               [add-history (-> String Void)])

(define (READ [s : String]) : String
  s)

(define (EVAL [s : String]) : String
  s)

(define (PRINT [s : String]) : String
  s)

(define (rep [s : String]) : String
  (PRINT (EVAL (READ s))))

(define (repl) : Void
    (define s (readline "user> "))
    (unless (eq? s eof)
        (cond
            [(> (string-length s) 0)
                (add-history s)
                (displayln (rep s))])
        (repl)))

(repl)
