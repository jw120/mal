#lang racket

(provide env%)

(require "exceptions.rkt")

;; class for environments
(define env%
  (class object%
    (init [outer #f])
    (define data (make-hash)) ; mutable hash
    (define my-outer outer)
    (super-new)
    (define/public (set key val)
      (hash-set! data key val))
    (define/public (find key)
      (cond
        [(hash-has-key? data key) this]
        [(equal? my-outer #f) my-outer]
        [else (send my-outer find key)]))
    (define/public (get key)
      (cond
        [(hash-has-key? data key) (hash-ref data key)]
        [(equal? my-outer #f) (raise-mal-eval (format "~a not found" key))]
        [else  (send my-outer get key)]))))
