#lang typed/racket/base

(provide (all-defined-out))

(define-type Mal (U
                  Integer
                  String
                  Symbol
                  Boolean
                  ;(Boxof Mal)
                  Void ; used to signal no value (not shown in repl)
                  mal-nil
                  mal-keyword
                  mal-list
                  mal-vector
                  mal-hash
                  mal-function))
                  ;mal-macro))

(struct mal-env ([data : (HashTable Symbol Mal)] [outer : (U mal-env #f)]) #:transparent #:mutable)

(struct mal-nil () #:transparent)
(struct mal-keyword ([s : String]) #:transparent)
(struct mal-list ([xs : (Listof Mal)]) #:transparent)
(struct mal-vector ([v : (Immutable-Vectorof Mal)]) #:transparent)
(define-type MalHashKey (U String mal-keyword))
(struct mal-hash ([m : (Immutable-HashTable MalHashKey Mal)]) #:transparent)
(struct mal-function ([f : (-> (Listof Mal) Mal)]) #:transparent)
;(struct mal-macro ([f : (-> (Listof Mal) Mal)]))

(struct mal-list-with-meta mal-list ([meta : Mal]) #:transparent)
(struct mal-vector-with-meta mal-vector ([meta : Mal]) #:transparent)
(struct mal-hash-with-meta mal-hash ([meta : Mal]) #:transparent)
(struct mal-function-with-meta mal-function ([meta : Mal]) #:transparent)

;; exception raised when no input is provided (when caught, repl ignores the input line)
(struct exn:mal-empty exn:fail ()) ; subtype of `exn:mal`
(define (raise-mal-empty) : Nothing
  (raise (exn:mal-empty "Empty input to reader" (current-continuation-marks))))

;; normal exception thrown from mal (when caught, repl prints the thrown value)
(struct exn:mal exn:fail ([thrown-value : Mal])) ; subtype of `exn:mal`
(define (raise-mal [val : Mal]) : Nothing
  (raise (exn:mal "Thrown value" (current-continuation-marks) val)))
(define (raise-mal-failure [msg : String]) : Nothing ; Internal inconsistency (not an error in mal code)
  (raise (exn:mal "Internal failure" (current-continuation-marks) msg)))

