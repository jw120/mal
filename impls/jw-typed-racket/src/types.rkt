#lang typed/racket/base


(provide Mal)

(define-type Mal (U
                  Integer
                  String
                  Symbol
                  Boolean
                  MalNil
                  MalList
                  MalVector
                  MalHash
                  MalFunction
                  MalMacro
                  MalAtom))

(struct MalNil ())

(struct MalList ([meta : Mal]
                 [xs : (Listof Mal)]))

(struct MalVector ([meta : Mal]
                   [v : (Immutable-Vectorof Mal)]))

(struct MalHash ([meta : Mal]
                 [m : (Immutable-HashTable String Mal)]))

(struct MalFunction ([meta : Mal]
                     [f : (-> (Listof Mal) Mal)]))

(struct MalMacro ([meta : Mal]
                  [f : (-> (Listof Mal) Mal)]))

(struct MalAtom ())