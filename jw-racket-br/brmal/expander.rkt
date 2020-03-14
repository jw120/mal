#lang br/quicklang

; Expanded module for brmal language

(provide (rename-out [my-module-begin #%module-begin])
         (all-defined-out))

(define-macro (my-module-begin SEXP)
  #'(#%module-begin ;; from `brmal`
     SEXP))

(define-macro (mal-expr EXPR) #'EXPR)
(define-macro (mal-list EXPR ...) #`(list EXPR ...))
(define-macro (mal-vec EXPR ...) #`(vector EXPR ...))
(define-macro (mal-map EXPR ...) #`(hash-map EXPR ...))
(define-macro (mal-special SYM) #'SYM)
(define-macro (mal-sym SYM) #'SYM)
(define-macro (mal-error ERR) #'ERR)
