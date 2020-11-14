#lang typed/racket

(provide EVAL)

(require "env.rkt" "printer.rkt" "types.rkt")

(define (EVAL [eval-ast : Mal] [env : mal-env]) : Mal
  (match (macro-expand eval-ast env)

    ;; def! special form
    [(mal-list (list 'def! (? symbol? sym) val))
     (let ([evaluated-val (EVAL val env)])
       (env-set! env sym evaluated-val)
       evaluated-val)]

    ;; defmacro! special form
    [(mal-list (list 'defmacro! (? symbol? sym) val))
     (match (EVAL val env)
       [(mal-function f)
        (env-set! env sym (mal-macro f))
        (mal-macro f)]
       [_
        (raise-mal "defmacro! needs a symbol and a function")])]
    
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

    ;; macroexpand special form
    [(mal-list (list 'macroexpand x))
     (macro-expand x env)]
    
    ;; quasiquote special form
    [(mal-list (list 'quasiquote x))
     (EVAL (mal-quasiquote x) env)]

    ;; quasiquoteexpand special form
    [(mal-list (list 'quasiquoteexpand x))
     (mal-quasiquote x)]
    
    ;; quote special form
    [(mal-list (list 'quote x))
     x]

    ;; try-catch special form
    [(mal-list (list 'try* a (mal-list (list 'catch* b c))))
     (define (handle [x : Mal]) : Mal
       (let ([catch-env (env-new-from-lists (list b) (list x) env)])
         (EVAL c catch-env)))
     (with-handlers
         ([exn:mal? (λ ([e : exn:mal]) (handle (exn:mal-thrown-value e)))]
          [exn:fail? (λ ([e : exn:fail]) (handle (exn-message e)))])
       (EVAL a env))]
    [(mal-list (list 'try single-arg))
     (EVAL single-arg env)]
    
    ;; If not a special form, apply a non-empty list, hand anything else to eval_ast
    [x
     (cond
       [(and (mal-list? x) (not (null? (mal-list-xs x))))
        (match (eval_ast x env)
          [(mal-list (cons (mal-function f) other-args))
           (f other-args)]
          [_ (raise-mal "cannot apply non-function")])]
       [else (eval_ast x env)])]))

(define (eval_ast [ast : Mal] [env : mal-env]) : Mal
  (define (eval-with-env [x : Mal]) : Mal
    (EVAL x env))
  (match ast
    [(? mal-nil? _)
     ast]
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

(define (macro-expand [ast : Mal] [env : mal-env]) : Mal
  ;  (display "macro-expand: ") (displayln ast)
  (match (get-macro ast env)
    [(cons m args)
     ;     (display "Found macro: ") (display m) (display " on ") (displayln args)
     (macro-expand (m args) env)]
    [_
     ast]))

(define (mal-quasiquote [ast : Mal]) : Mal
  (match ast
    [(mal-vector v)
     (mal-list (list 'vec (mal-qq-list (vector->list v))))]
    [(mal-hash h)
     (mal-list (list 'quote ast))]
    [(? symbol? s)
     (mal-list (list 'quote s))]
    [(mal-list (list 'unquote x))
     x]
    [(mal-list '())
     ast]
    [(mal-list xs)
     (mal-qq-list xs)]
    [_ 
     ast]))

(define (mal-qq-list [list-vals : (Listof Mal)]) : Mal
  (match list-vals
    [(list (mal-list (list 'splice-unquote val)) rest ...)
     (mal-list (list 'concat val (mal-qq-list rest)))]
    [(list val rest ...)
     (mal-list (list 'cons (mal-quasiquote val) (mal-qq-list rest)))]
    ['()
     (mal-list '())]
    [_
     (raise-mal-failure "unexpected value in mal-qq-list")]))
  
 
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


;; Helper function that returns the macro closure and other arguments
;; if the ast is a mal list whose first element is a symbol has a macro value in the environment
(define (get-macro [ast : Mal] [env : mal-env]) : (U #f (Pair (-> (Listof Mal) Mal) (Listof Mal)))
  (match ast
    [(mal-list (list (? symbol? s) args ...))
     (let ([found-env (env-find env s)])
       (if found-env
           (match (env-get found-env s)
             [(mal-macro m) (cons m args)]
             [_ #f])
           #f))]
    [_ #f]))
(module+ test
  (require typed/rackunit)
  (let ([e (env-new '() #f)]
        [m (lambda ([args : (Listof Mal)]) (mal-list args))])
    (env-set! e 's1 (mal-macro m))
    (env-set! e 's2 2)
    (check-equal? (get-macro (mal-list (list 's1 2 3)) e) (cons m '(2 3)))
    (check-equal? (get-macro (mal-list (list 's1)) e) (cons m '()))
    (check-false (get-macro (mal-list (list 's2)) e))
    (check-false (get-macro 3 e))
    (check-false (get-macro (mal-list '()) e))))

