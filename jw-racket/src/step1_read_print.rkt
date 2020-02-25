#lang racket
(require readline readline/readline)
(require "exceptions.rkt" "printer.rkt" "reader.rkt") 

(define (READ s) (read_string s))

(define (EVAL s) s)

(define (PRINT s) (pr_str s))

(define (rep s)
  (with-handlers ([exn:mal:read? (lambda (exn) (displayln (exn-message exn)))])
    (PRINT (EVAL (READ s)))))

(define (repl)
    (define s (readline "user> "))
    (unless (eq? s eof)
        (cond
            [(> (string-length s) 0)
                (add-history s)
                (displayln (rep s))])
        (repl)))

(repl)
