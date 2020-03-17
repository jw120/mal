#lang racket/base

(require brmal/exceptions)

(provide + - / * define          (all-defined-out)
)

(define keyword-prefix "\u029e")

(define-syntax-rule (mal-expr x) x)
(define-syntax mal-list
  (syntax-rules ()
    [(m-list) '()]
    [(m-list x ...) (x ...)]))
(define-syntax-rule (mal-vec x ...) (vector-immutable x ...))
(define-syntax-rule (mal-map x ...) (hash x ...))
(define-syntax-rule (mal-special1 s x)
  (cond
    [(equal? s "'") `x]
    [(equal? s "`") `x]
    [(equal? s "~") ~x]
    [(equal? s "^") ^x]
    [(equal? s "@") @x]
    [(equal? s "~@") ~@x]
    [else (raise-mal-fail "unknown special in special1")]))
(define-syntax-rule (mal-sym s) s)
(define-syntax-rule (mal-keyword s) (string-append-immutable keyword-prefix s))
(define-syntax-rule (mal-error-eof open x ...)
  (cond
    [(equal? open "(") (raise-mal-read "EOF found parsing a list")]
    [(equal? open "[") (raise-mal-read "EOF found parsing a vector")]
    [(equal? open "{") (raise-mal-read "EOF found parsing a hash-map")]
    [else (raise-mal-read "EOF found parsing a string")]))

(define-syntax-rule (def! a b) (define a b))
