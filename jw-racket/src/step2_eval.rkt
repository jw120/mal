#lang racket
(require readline readline/readline)
(require "exceptions.rkt" "printer.rkt" "reader.rkt" "repl.rkt")

(define (READ s) (read_string s))

(define (EVAL ast env)
  (cond
    [(not (list? ast)) (eval_ast ast env)]
    [(null? ast) ast]
    [else ((eval_ast ast env))]))

(define (eval_ast ast env)
  (cond
    [(symbol? ast) (hash-ref env ast (raise-mal-eval (format "Symbol ~a not found" ast)))]
    [(list? ast) (map (λ (x) (EVAL x env)) ast)]
    [else ast]))

(define (PRINT s) (pr_str s #t))

(define (rep s env)
  (PRINT (EVAL (READ s) env)))

(define repl_env (hash '+ + '- - '* * '/ /))

(repl (λ (s) (rep s repl_env)))
