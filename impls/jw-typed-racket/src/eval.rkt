#lang typed/racket

(provide EVAL)

(require "env.rkt" "types.rkt")

(define (EVAL [ast : Mal] [env : mal-env]) : Mal
  (match ast
    [(mal-list '()) ast]
    [(mal-list xs)
     (match (eval_ast ast env)
       [(mal-list (cons (mal-function f) other-args))
        (f other-args)]
       [_ (raise-mal "cannot apply non-function")])]
    [_ (eval_ast ast env)]))

(define (eval_ast [ast : Mal] [env : mal-env]) : Mal
  (define (eval-with-env [x : Mal]) : Mal
    (EVAL x env))
  (match ast
    [(? symbol? sym)
     (env-get env sym)]
    [(mal-list xs)
     (mal-list (map eval-with-env xs))]
    [(mal-vector v)
     (mal-vector (vector->immutable-vector (vector-map eval-with-env v)))]
    [_ ast]))