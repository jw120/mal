#lang racket

(require (for-syntax syntax/parse  brmal/exceptions))

(provide + - / * define        (all-defined-out)
)

(define keyword-prefix "\u029e")

(define-syntax (mal-expr stx)
  (syntax-parse stx
    [(_ x:expr) #'x]))

(define-syntax (mal-def! stx)
  (syntax-parse stx
    [(_ (~datum "def!") a:expr b:expr) #'(begin (define a b) b)]))

(define-syntax (mal-list stx)
  (syntax-parse stx
    [(_) #''()]
    [(_ x:expr ...) #'(x ...)]))

(define-syntax (mal-vec stx)
  (syntax-parse stx
    [(_ x:expr ...) #'(vector-immutable x ...)]))

(define-syntax (mal-map stx)
  (syntax-parse stx
    [(_ x:expr ...) #'(hash x ...)]))

(define-syntax (mal-special1 stx)
  (syntax-parse stx
    [(_ (~datum "'") x:expr) #''x]
    [(_ (~datum "`") x:expr) #'`x]
    [(_ (~datum "~") x:expr) #'~x]
    [(_ (~datum "^") x:expr) #'^x]
    [(_ (~datum "@") x:expr) #'@x]
    [(_ (~datum "~@") x:expr) #'~@x]))

(define-syntax (mal-sym stx)
  (syntax-parse stx
    [(_ s:expr) #'s]))

(define-syntax (mal-keyword stx)
  (syntax-parse stx
    [(_ s:string) #'(string-append-immutable keyword-prefix s)]))

(define-syntax (mal-error-eof stx)
  (syntax-parse stx
    [(_ (~datum "(")) (raise-mal-read "EOF found parsing a list")]
    [(_ (~datum "[")) (raise-mal-read "EOF found parsing a vector")]
    [(_ (~datum "{")) (raise-mal-read "EOF found parsing a map")]
    [(_ _) (raise-mal-read "EOF found parsing a string")]))
