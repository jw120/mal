#lang racket
(require readline readline/readline)

(define (READ s) s)

(define (EVAL s) s)

(define (PRINT s) s)

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
