#lang racket
(require readline readline/readline)
(require "env.rkt" "exceptions.rkt" "printer.rkt" "reader.rkt" "repl.rkt" "utils.rkt")

(define (READ s) (read_string s))

(define (EVAL ast env)
  (cond
    [(not (list? ast)) (eval_ast ast env)]
    [(null? ast) ast]
    [else
     (let ([head (car ast)]
           [args (cdr ast)])
       (match head
         ['def! (def-special-form args env)]
         ['let* (let-special-form args env)]
         [else 
          (let* ([evaluated-ast (eval_ast ast env)]
                 [evaluated-head (car evaluated-ast)]
                 [evaluated-args (cdr evaluated-ast)])
            (cond
              [(procedure? evaluated-head) (apply evaluated-head evaluated-args)]
              [else (raise-mal-eval "Cannot apply non-procedure")]))]))]))

(define (def-special-form args env)
  (unless (and (equal? 2 (length args)) (symbol? (car args)))
    (raise-mal-eval "Bad arguments to def!"))
  (let ([sym (car args)]
        [val (EVAL (cadr args) env)])
    (send env set sym val)
    val))

(define (let-special-form args env)
  (unless (and (equal? 2 (length args)) (list-or-vector? (car args)))
    (raise-mal-eval "Bad arguments to let*"))
  (let ([binding-list (list-or-vector->list (car args))]
        [val (cadr args)]
        [let-env (new env% [outer env])])
    (bind-alternating-list let-env binding-list)
    (EVAL val let-env)))

; Helper function: bind the elements in alternating (sym val sym val) list to the environment
(define (bind-alternating-list env lst)
  (unless (empty? lst)
    (when (empty? (cdr lst))
      (raise-mal-eval (format "Bad bind in let*: ~a" (car lst))))
    (send env set (car lst) (EVAL (cadr lst) env))
    (bind-alternating-list env (cddr lst))))
  
(define (eval_ast ast env)
  (cond
    [(symbol? ast) (send env get ast)]
    [(list? ast) (map (λ (x) (EVAL x env)) ast)]
    [(vector? ast) (vector-map (λ (x) (EVAL x env)) ast)]
    [(hash? ast) (make-immutable-hash (map (λ (k) (cons k (EVAL (hash-ref ast k) env))) (hash-keys ast)))]
    [else ast]))

(define (PRINT s) (pr_str s #t))

(define (rep s env)
  (PRINT (EVAL (READ s) env)))

(define repl_env (new env%))
(send repl_env set '+ +)
(send repl_env set '- -)
(send repl_env set '* *)
(send repl_env set '/ /)

(repl (λ (s) (rep s repl_env)))
