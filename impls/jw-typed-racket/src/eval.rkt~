#lang typed/racket

(provide EVAL)

(require "env.rkt" "types.rkt")

(define (EVAL [ast : Mal] [env : mal-env]) : Mal
  (match ast

    ;; def! special form
    [(mal-list (list 'def! (? symbol? sym) val))
     (let ([evaluated-val (EVAL val env)])
       (env-set! env sym evaluated-val)
       evaluated-val)]

    ;; do special form
    [(mal-list (list 'do args ...))
     (for/last : Mal ([a : Mal args])
       (EVAL a env))]
    
    ;; let* special form
    [(mal-list (list 'let* (mal-list bindings) let-ast))
     (let*-special-form bindings let-ast env)]
    [(mal-list (list 'let* (mal-vector bindings-vector) let-ast))
     (let*-special-form (vector->list bindings-vector) let-ast env)]

    ;; non-empty list with apply
    [(mal-list (cons _ _))
     (match (eval_ast ast env)
       [(mal-list (cons (mal-function f) other-args))
        (f other-args)]
       [_ (raise-mal "cannot apply non-function")])]

    ;; anything else handed off to eval_ast
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
    [(mal-hash m)
     (mal-hash
      (for/hash : (Immutable-HashTable MalHashKey Mal) ([k : MalHashKey (hash-keys m)])
        (values k (eval-with-env (hash-ref m k)))))]
    [_ ast]))

(define (let*-special-form [bindings : (Listof Mal)] [ast : Mal] [env : mal-env]) : Mal
  (let ([let-env (env-new '() env)])
    (add-bindings! let-env bindings)
    (EVAL ast let-env)))

;; Helper function to add bindings (x 2 y (+ 3 4)... to an environment
(define (add-bindings! [env : mal-env] [binding-list : (Listof Mal)]) : Void
  (cond [(null? binding-list)
         (void)]
        [(null? (cdr binding-list))
         (raise-mal "Odd number of elements in binding-list")]
        [(symbol? (car binding-list))
         (env-set! env (car binding-list) (EVAL (cadr binding-list) env))
         (add-bindings! env (cddr binding-list))]
        [else
         (raise-mal "binding list keys must be symbols")]))
