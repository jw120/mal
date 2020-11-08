#lang racket/base

(require racket/class
         racket/list
         racket/match
         racket/vector
         "core.rkt"
         "env.rkt"
         "exceptions.rkt"
         "printer.rkt"
         "reader.rkt"
         "repl.rkt"
         "utils.rkt")

(define (READ s) (read_string s))

(define (EVAL ast env)
  (cond
    [(not (list? ast)) (eval_ast ast env)]
    [(null? ast) ast]
    [else
     (define expanded-ast (macro-expand ast env))
     (if (or (not (list? expanded-ast)) (null? expanded-ast))
         (eval_ast expanded-ast env)
         (let ([head (car expanded-ast)]
               [args (cdr expanded-ast)])
           (match head
             ['def! (def-special-form args env)]
             ['defmacro! (defmacro-special-form args env)]
             ['do (do-special-form args env)]
             ['fn* (fn-special-form args env)]
             ['if (if-special-form args env)]
             ['let* (let-special-form args env)]
             ['macroexpand (macroexpand-special-form args env)]
             ['quasiquote (EVAL (mal-quasi-quote (car args)) env)]
             ['quasiquoteexpand (mal-quasi-quote (car args))]
             ['quote (quote-special-form args env)]
             [else
              (let* ([evaluated-ast (eval_ast expanded-ast env)]
                     [evaluated-head (car evaluated-ast)]
                     [evaluated-args (cdr evaluated-ast)])
                (cond
                  [(procedure? evaluated-head) (apply evaluated-head evaluated-args)]
                  [(func? evaluated-head) (apply (func-closure evaluated-head) evaluated-args)]
                  [else (raise-mal-eval "Cannot apply non-procedure")]))])))]))

(define (def-special-form args env)
  (unless (and (equal? 2 (length args)) (mal-symbol? (car args)))
    (raise-mal-eval "Bad arguments to def!"))
  (let ([sym (car args)]
        [val (EVAL (cadr args) env)])
    (send env set sym val)
    val))

(define (defmacro-special-form args env)
  (unless (and (equal? 2 (length args)) (mal-symbol? (car args)))
    (raise-mal-eval "Bad arguments to defmacro!"))
  (let ([sym (car args)]
        [val (EVAL (cadr args) env)])
    (unless (func? val)
      (raise-mal-eval "Bad value for defmacro!"))
    (let ([macro-val (struct-copy func val [is-macro? #t])])
      (send env set sym macro-val)
      macro-val)))

(define (do-special-form args env)
  (when (empty? args)
    (raise-mal-eval "No arguments to do"))
  (for/last ([a args])
    (EVAL a env)))

(define (fn-special-form args env)
  (match args
    [(list fn-binds fn-val)
     (func
      #f
      (lambda closure-args
        (let ([closure-env (new env% [outer env] [binds (list-or-vector->list fn-binds)] [exprs closure-args])])
          (EVAL fn-val closure-env)))
      nil)]
    [_ (raise-mal-eval "Bad arguments to fn*")]))

(define (if-special-form args env)
  (unless (or (equal? (length args) 2) (equal? (length args) 3))
    (raise-mal-eval "Bad arguments to if"))
  (define test-value (EVAL (first args) env))
  (if (or (equal? test-value nil) (equal? test-value #f))
      (if (equal? (length args) 2) nil (EVAL (third args) env))
      (EVAL (second args) env)))

(define (let-special-form args env)
  (unless (and (equal? 2 (length args)) (list-or-vector? (car args)))
    (raise-mal-eval "Bad arguments to let*"))
  (let ([binding-list (list-or-vector->list (car args))]
        [val (cadr args)]
        [let-env (new env% [outer env])])
    (send let-env bind-alternating-list binding-list EVAL)
    (EVAL val let-env)))

(define (macroexpand-special-form args env)
  (unless (equal? 1 (length args))
    (raise-mal-eval "Bad arguments to macroexpand"))
  (macro-expand (car args) env))

(define (quote-special-form args env)
  (when (empty? args)
    (raise-mal-eval "No arguments to quote"))
  (car args))

(define (eval_ast ast env)
  (cond
    [(nil? ast) ast]
    ((boolean? ast) ast)
    [(mal-symbol? ast) (send env get ast)]
    [(list? ast) (map (λ (x) (EVAL x env)) ast)]
    [(vector? ast) (vector-map (λ (x) (EVAL x env)) ast)]
    [(hash? ast) (make-immutable-hash (map (λ (k) (cons k (EVAL (hash-ref ast k) env))) (hash-keys ast)))]
    [else ast]))

(define (mal-quasi-quote ast)
  (cond
    [(vector? ast)
     (list 'vec (mal-qq-list (vector->list ast)))]
    [(or (hash? ast) (mal-symbol? ast)) ; Non-sequence types that need quoting
     (list 'quote ast)]
    [(not (pair? ast)) ; Non-sequence types that are self-quoting
     ast]
    [(null? ast) ; Empty list
     '()]
    [(and (equal? (car ast) 'unquote) (not (null? (cdr ast)))) ; Handle quote
     (cadr ast)]
    [else ; Normal list processing
     (mal-qq-list ast)]))

(define (mal-qq-list ast) ; Helper function when ast is a list
  (if (null? ast)
      '()
      (let ([head (car ast)]
            [rest (cdr ast)])
        (cond [(and (pair? head) (equal? (car head) 'splice-unquote))
               (list 'concat (cadr head) (mal-qq-list rest))]
              [else
               (list 'cons (mal-quasi-quote head) (mal-qq-list rest))]))))


(define (is-macro-call ast env)
  (cond
    [(and (list? ast) (mal-symbol? (car ast)) (send env find (car ast)))
     (let ([val (send env get (car ast))])
       (and
        (func? val)
        (func-is-macro? val)))]
    [else #f]))

(define (macro-expand ast env)
  (if (is-macro-call ast env)
      (let ([macro-func (send env get (car ast))]
            [args (cdr ast)])
        (macro-expand (apply (func-closure macro-func) args) env))
      ast))

(define (PRINT s) (pr_str s #t))

(define repl_env (new env%))
(define (rep s) (PRINT (EVAL (READ s) repl_env)))

; Add bindings from ns (from core)
(for ([binding-pair ns])
  (send repl_env set (car binding-pair) (cdr binding-pair)))

; Add additonal bindings
(send repl_env set 'eval (λ (ast) (EVAL ast repl_env)))
(send repl_env set '*ARGV* '())

; Add mal-defined functions
(define mal-defs
  '("(def! not (fn* (a) (if a false true)))"
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\\nnil)\")))))"
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) (if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"))

(for ([d mal-defs])
  (rep d))

; Either start the repl or load the given file
(cond
  [(vector-empty? (current-command-line-arguments))
   (repl rep)]
  [else
   (send repl_env set '*ARGV* (vector->list (vector-drop (current-command-line-arguments) 1)))
   (rep (format "(load-file ~s)" (vector-ref (current-command-line-arguments) 0)))
   (void)]) ; void to avoid returning the value from rep
