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

(struct mal-nil () #:transparent)
(struct mal-list ([xs : (Listof Mal)]) #:transparent)
(struct mal-vector ([v : (Immutable-Vectorof Mal)]) #:transparent)
(struct mal-hash ([m : (Immutable-HashTable String Mal)]) #:transparent)
(struct mal-function ([f : (-> (Listof Mal) Mal)]) #:transparent)
;(struct mal-macro ([f : (-> (Listof Mal) Mal)]))

(struct mal-list-with-meta mal-list ([meta : Mal]) #:transparent)
(struct mal-vector-with-meta mal-vector ([meta : Mal]) #:transparent)
(struct mal-hash-with-meta mal-hash ([meta : Mal]) #:transparent)
(struct mal-function-with-meta mal-function ([meta : Mal]) #:transparent)

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



