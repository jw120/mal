#lang racket
(require readline readline/readline)
(require "printer.rkt" "reader.rkt") 

(define (READ s) (read_string s))

(define (EVAL s) s)

(define (PRINT s) (pr_str s))

(define (rep s) (PRINT (EVAL (READ s))))

(define (repl)
    (define s (readline "user> "))
    (unless (eq? s eof)
        (cond
            [(> (string-length s) 0)
                (add-history s)
                (displayln (rep s))])
        (repl)))

(repl)
