#lang br

(provide + - / *          (all-defined-out)
)

(define-macro (mal-expr EXPR) #'EXPR)
(define-macro (mal-list EXPR ...) #'(EXPR ...))
(define-macro (mal-vec EXPR ...) #'(vector EXPR ...))
(define-macro (mal-map EXPR ...) #'(hash-map EXPR ...))
(define-macro (mal-special SPECIAL) #'SPECIAL)
(define-macro (mal-sym SYM) #'SYM)
(define-macro (mal-error ERR) #'ERR)
