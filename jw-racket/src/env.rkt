#lang racket/base

(provide env%)

(require racket/class
         racket/list
         racket/match
         "exceptions.rkt")

;; class for environments (wrapper for a mutable hash with an outer chain)
(define env%
  (class object%

    (init-field [outer #f] [binds '()] [exprs '()])
    (super-new)
    (define data (make-hash))
    (set-binds binds exprs)

    (define/private (set-binds binds exprs)
     (match (list binds exprs)
        [(list (list '& x) y)
         (hash-set! data x y)]
        [(list (cons x xs) (cons y ys))
         (hash-set! data x y)
         (set-binds xs ys)]
        [(list '() '()) void]
        [_ raise-mal-eval "Bad bind lists for environment"]))

    (define/public (set key val)
      (hash-set! data key val))

    (define/public (find key)
      (cond
        [(hash-has-key? data key) this]
        [(equal? outer #f) outer]
        [else (send outer find key)]))

    (define/public (get key)
      (cond
        [(hash-has-key? data key) (hash-ref data key)]
        [(equal? outer #f) (raise-mal-eval (format "~a not found" key))]
        [else  (send outer get key)]))

    (define/public (bind-alternating-list lst eval-fn)
      (unless (empty? lst)
        (when (empty? (cdr lst))
          (raise-mal-eval (format "Bad alternating-binding-list: ~a" (car lst))))
        (set (car lst) (eval-fn (cadr lst) this))
        (bind-alternating-list (cddr lst) eval-fn)))))


(module+ test
  (require rackunit)
  (let*
      ([top (new env% [binds '("a" "b")] [exprs '(2 3)])]
       [sub (new env% [outer top])]
       [amp (new env% [binds (list 'a '& 'b)] [exprs (list 1 2 3 4)])])
    (send sub set "a" 5)
    (send sub set "c" 8)

    (check-equal? (send top get "a") 2 "Top top a")
    (check-equal? (send top get "b") 3 "Top top b")
    (check-exn exn:mal:eval? (λ () (send top get "c")) "Top missing")
    (check-equal? (send sub get "a") 5 "Sub sub a")
    (check-equal? (send sub get "b") 3 "Sub top b")
    (check-equal? (send sub get "c") 8 "Sub sub c")

    (check-equal? (send sub find "a") sub "Find sub sub a")
    (check-equal? (send sub find "b") top "Find sub top b")
    (check-equal? (send sub find "c") sub "Find sub sub c")
    (check-equal? (send sub find "d") #f "Find sub sub missing")

;    (check-equal? (send amp get "a") 1 "Ampersand first")
    ;    (check-equal? (send amp get "b") '(2 3 4) "Ampersand rest")
    ))
