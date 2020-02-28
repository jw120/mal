#lang racket

(provide env%)

(require "exceptions.rkt")

;; class for environments (wrapper for a mutable hash with an outer chain)
(define env%
  (class object%
  
    (init-field [outer #f] [binds '()] [exprs '()])
    (define data (make-hash))
    (unless (equal? (length binds) (length exprs))
      (raise-mal-eval "Mistmatched binding and expressions"))
    (for ([b binds] [e exprs])
      (hash-set! data b e))
    (super-new)
    
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
