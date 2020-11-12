#lang typed/racket

(provide EVAL)

(require "env.rkt" "printer.rkt" "types.rkt")

(define (EVAL [ast : Mal] [env : mal-env]) : Mal
  (match ast

    ;; def! special form
    [(mal-list (list 'def! (? symbol? sym) val))
     (let ([evaluated-val (EVAL val env)])
       (env-set! env sym evaluated-val)
       evaluated-val)]

    ;; do special form
    [(mal-list (list 'do))
     (void)]
    [(mal-list (list 'do args ...))
     (for/last : Mal ([a : Mal args])
       (EVAL a env))]

    ;; fn* special form
    [(mal-list (list 'fn* (mal-list binds) ast))
     (mal-function
      (lambda (calling-params)
        (let ([fn-env (env-new-from-lists binds calling-params env)])
          (EVAL ast fn-env))))]
    [(mal-list (list 'fn* (mal-vector binds) ast))
     (EVAL (mal-list (list 'fn* (mal-list (vector->list binds)) ast)) env)]
    
    ;; if special form
    [(mal-list (list 'if condition then-ast else-ast))
     (if (mal-truthy? (EVAL condition env))
         (EVAL then-ast env)
         (EVAL else-ast env))]
    [(mal-list (list 'if condition then-ast))
     (EVAL (mal-list (list 'if condition then-ast (mal-nil))) env)]
    
    ;; let* special form
    [(mal-list (list 'let* (mal-list bindings) let-ast))
     (let ([let-env (env-new '() env)])
       (add-bindings! let-env bindings)
       (EVAL let-ast let-env))]
    [(mal-list (list 'let* (mal-vector bindings-vector) let-ast))
     (EVAL (mal-list (list 'let* (mal-list (vector->list bindings-vector)) let-ast)) env)]

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

