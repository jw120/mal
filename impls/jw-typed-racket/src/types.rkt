#lang typed/racket/base

(provide (all-defined-out))

(define-type Mal (U
                  Integer
                  String
                  Symbol
                  Boolean
                  ;(Boxof Mal)
                  mal-nil
                  mal-list
                  mal-vector
                  mal-hash
                  mal-function))
                  ;mal-macro))

(struct mal-nil ())

(struct mal-with-meta ([meta : Mal]))

(struct mal-list mal-with-meta ([xs : (Listof Mal)]))

(struct mal-vector mal-with-meta ([v : (Immutable-Vectorof Mal)]))

(struct mal-hash mal-with-meta ([m : (Immutable-HashTable String Mal)]))

(struct mal-function mal-with-meta ([f : (-> (Listof Mal) Mal)]))

;(struct mal-macro ([f : (-> (Listof Mal) Mal)]))

;; exception raised when no input is provided (when caught, repl ignores the input line)
(struct exn:mal-empty exn ()) ; subtype of `exn:mal`
(define (raise-mal-empty) : Nothing
  (raise (exn:mal-empty "Empty input to reader" (current-continuation-marks))))

;; exception thrown from mal throw (when caught, repl prints the thrown value)
(struct exn:mal-throw exn ([thrown-value : Mal])) ; subtype of `exn:mal`
(define (raise-mal-throw [val : Mal]) : Nothing
  (raise (exn:mal-throw "Thrown exception" (current-continuation-marks) val)))

;; other exceptions during evaluation
(struct exn:mal exn ())
(define (raise-mal-failure [msg : String]) : Nothing ; Internal inconsistency (not an error in mal code)
  (raise (exn:mal (string-append "Internal failure: " msg) (current-continuation-marks))))
(define (raise-mal [msg : String]) : Nothing
  (raise (exn:mal msg (current-continuation-marks))))



