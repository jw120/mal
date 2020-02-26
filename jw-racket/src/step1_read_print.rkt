#lang racket
(require readline readline/readline)
(require "exceptions.rkt" "printer.rkt" "reader.rkt" "repl.rkt") 

(define (READ s) (read_string s))

(define (EVAL s) s)

(define (PRINT s) (pr_str s))

(define (rep s)       
  (PRINT (EVAL (READ s))))

(repl rep)
