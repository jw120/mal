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

