#lang typed/racket/base

(provide (all-defined-out))

(require "types.rkt")

(define (env-new [init-pairs : (Listof (Pair Symbol Mal))] [outer : (U mal-env #f)]) : mal-env
  (mal-env (make-hash init-pairs) outer))

(define (env-set! [env : mal-env] [key : Symbol] [val : Mal]) : Void
  (hash-set! (mal-env-data env) key val))

(define (env-find [env : mal-env] [key : Symbol]) : (U mal-env #f)
  (if (hash-has-key? (mal-env-data env) key)
      env
      (let ([outer (mal-env-outer env)])
        (if outer
            (env-find outer key)
            #f))))

(define (env-get [env : mal-env] [key : Symbol]) : Mal
  (let ([found-env (env-find env key)])
    (if found-env
        (hash-ref (mal-env-data found-env) key)
        (raise-mal (string-append "Cannot find " (symbol->string key))))))


  #|


    (define/private (set-binds binds exprs)
     (match (list binds exprs)
        [(list (list '& x) y)
         (hash-set! data x y)]
        [(list (cons x xs) (cons y ys))
         (hash-set! data x y)
         (set-binds xs ys)]
        [(list '() '()) void]
        [_ raise-mal-eval "Bad bind lists for environment"]))

    (define/public (bind-alternating-list lst eval-fn)
      (unless (empty? lst)
        (when (empty? (cdr lst))
          (raise-mal-eval (format "Bad alternating-binding-list: ~a" (car lst))))
        (set (car lst) (eval-fn (cadr lst) this))
        (bind-alternating-list (cddr lst) eval-fn)))))

|#

(module+ test
  (require typed/rackunit)
  (let*
      ([top (env-new '((a . 2) (b . 3)) #f)]
       [sub (env-new '() top)])
    (env-set! sub 'a 5)
    (env-set! sub 'c 8)

    (check-equal? (env-get top 'a) 2 "Top top a")
    (check-equal? (env-get top 'b) 3 "Top top b")
    (check-exn exn:mal? (λ () (env-get top 'c)) "Top missing")
    (check-equal? (env-get sub 'a) 5 "Sub sub a")
    (check-equal? (env-get sub 'b) 3 "Sub top b")
    (check-equal? (env-get sub 'c) 8 "Sub sub c")

    (check-equal? (env-find sub 'a) sub "Find sub sub a")
    (check-equal? (env-find top 'b) top "Find sub top b")
    (check-equal? (env-find sub 'c) sub "Find sub sub c")
    (check-equal? (env-find sub 'd) #f "Find sub sub missing")
    ))
  