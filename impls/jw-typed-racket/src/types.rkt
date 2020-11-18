#lang typed/racket

(provide (all-defined-out))

(define-type Mal (U
                  Integer
                  String
                  Symbol ; Includes mal-nil
                  Boolean
                  (Boxof Mal)
                  Void ; used to signal no value (not shown in repl)
                  mal-nil
                  mal-keyword
                  mal-list
                  mal-vector
                  mal-hash
                  mal-function
                  mal-macro))

(struct mal-env ([data : (HashTable Symbol Mal)] [outer : (U mal-env #f)]) #:transparent #:mutable)

(struct mal-nil () #:transparent)
(struct mal-keyword ([s : String]) #:transparent)

; Attributes added (as a superclass) to types which need to carry meta values
(struct meta ([has-meta : Boolean] [meta : Mal]) #:transparent)

(struct mal-list meta ([xs : (Listof Mal)]) #:transparent)
(struct mal-vector meta ([v : (Immutable-Vectorof Mal)]) #:transparent)

(define-type MalHashKey (U String mal-keyword))
(: mal-hashkey? (-> Any Boolean : MalHashKey))
(define (mal-hashkey? x) (or (string? x) (mal-keyword? x)))
(struct mal-hash meta ([m : (Immutable-HashTable MalHashKey Mal)]) #:transparent)

(struct mal-function meta ([f : (-> (Listof Mal) Mal)]) #:transparent)
(struct mal-macro ([m : (-> (Listof Mal) Mal)]) #:transparent)

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

(define (mal-truthy? [x : Mal]) : Boolean
  (match x
    [#f #f]
    [(mal-nil) #f]
    [_ #t]))

(module+ test
  (require typed/rackunit)
  
  (check-false (mal-truthy? #f))
  (check-false (mal-truthy? (mal-nil)))
  (check-true (mal-truthy? #t))
  (check-true (mal-truthy? 'x))
  (check-true (mal-truthy? 0)))
