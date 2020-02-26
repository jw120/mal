#lang racket

(provide env%)

(require "exceptions.rkt")

;; class for environments
(define env%
  (class object%
    (init [initial-outer #f])
    (define data (make-hash)) ; mutable hash
    (define outer initial-outer)
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
        [(equal? outer #f) (raise-mal-eval (format "Cannot find ~a in hash map" key))]
        [else  (send outer get key)]))))
