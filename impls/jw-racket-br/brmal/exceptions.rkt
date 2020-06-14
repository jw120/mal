#lang racket/base

(require racket/contract/base)

(provide (contract-out
          [raise-mal-empty (-> void?)]
          [raise-mal-eval (-> string? void?)]
          [raise-mal-fail (-> string? void?)]
          [raise-mal-read (-> string? void?)]
          [raise-mal-throw (-> any/c void?)])
         (struct-out exn:mal)
         (struct-out exn:mal:empty)
         (struct-out exn:mal:eval)
         (struct-out exn:mal:fail)
         (struct-out exn:mal:throw)
         (struct-out exn:mal:read))

;; our top-level exception (not raised directly)
(struct exn:mal exn ()) ; subtype of `exn`

;; exceptions raised when no input is provided
(struct exn:mal:empty exn:mal ()) ; subtype of `exn:mal`
(define (raise-mal-empty)
  (raise (exn:mal:empty "Empty input to reader" (current-continuation-marks))))

;; exceptions during evaluation (including exceptions raised by mal code)
(struct exn:mal:eval exn:mal ()) ; subtype of `exn:mal`
(define (raise-mal-eval msg)
  (raise (exn:mal:eval msg (current-continuation-marks))))

;; exceptions from an internal inconsistency
(struct exn:mal:fail exn:mal ()) ; subtype of `exn:mal`
(define (raise-mal-fail msg)
  (raise (exn:mal:fail msg (current-continuation-marks))))

;; exceptions during reading
(struct exn:mal:read exn:mal ()) ; subtype of `exn:mal`
(define (raise-mal-read msg)
  (raise (exn:mal:read msg (current-continuation-marks))))

;; exceptions thrown from mal throw
(struct exn:mal:throw exn:mal (thrown-value)) ; subtype of `exn:mal`
(define (raise-mal-throw val)
  (raise (exn:mal:throw "Thrown exception" (current-continuation-marks) val)))
