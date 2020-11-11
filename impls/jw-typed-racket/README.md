TODO
Done- Transparent types for structs
Done- Should we have a sub-type for list with meta? (then how do we tell if we have a meta value)
- Re-think how to track spaces and comments in reader. Just using whitespace string confused by "" or " " etc
- Get reader tests to pass

Can we have structs with only one variable is mutable

Should we add a mal-void type to return from def! and do with no args? Also avoids raising empty. Would not be printed for repl.


Typed racket version

Uses a sum type for mal (as opposed to my racket implementation that uses the underlying
dynamically typed racket value)

Will allow meta to be implemented


#lang typed/racket

;; List with a keyword as a tag

(define-type Mal1 (U
                   (List '#:mal-int Integer)
                   (List '#:mal-list (Listof Mal1) Mal1)))

(define (print1 [x : Mal1]) : String
  (match x
    [(list '#:mal-int n) (number->string n)]
    [(list '#:mal-list xs _) "List"]))

(print1 (list '#:mal-int 2))

;; Structs in hierarchy

(struct Mal2 ())
(struct Mal2-int Mal2 ([i : Integer]))
(struct Mal2-with-meta Mal2 ([meta : Mal2]))
(struct Mal2-list Mal2-with-meta ([xs : (Listof Mal2)]))

(define (print2 [x : Mal2]) : String
  (match x
    [(Mal2-int n) (number->string n)]
    [(Mal2-list meta xs) "List"]))

(print2 (Mal2-int 3))

;; Mix of structs and bare racket values

(define-type Mal3 (U Integer Mal3-list))

(struct Mal3-list ([meta : Mal3] [xs : (Listof Mal3)]))

(define (print3 [x : Mal3]) : String
  (match x
    [i #:when (exact-integer? i) (number->string i)]
    [(Mal3-list meta xs) "List"]))

(print3 4)