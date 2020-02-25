#lang racket

(provide (contract-out
          [raise-mal-eval (-> string? void?)]
          [raise-mal-fail (-> string? void?)]
          [raise-mal-read (-> string? void?)])
         (struct-out exn:mal)
         (struct-out exn:mal:eval)
         (struct-out exn:mal:fail)
         (struct-out exn:mal:read))

;; out top-level exception (not raised directly)
(struct exn:mal exn ()) ; subtype of `exn`

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
