#lang br

; Expanded module for brmal language

(require brmal/core)

(provide (rename-out [my-module-begin #%module-begin])
         (all-defined-out)
         (all-from-out brmal/core)
         #%app
         #%top
         #%top-interaction
         #%app
         #%datum)

(define-macro (my-module-begin SEXP)
  #'(#%module-begin ;; from brmal
;     (module configure-runtime racket/base
;       (displayln "In mb1")
;       (require brmal/setup)
;       (do-setup!))
;     (displayln "In mb2")
     SEXP))

(define-macro (mal-expr EXPR) #'EXPR)
(define-macro (mal-list EXPR ...) #'(EXPR ...))
(define-macro (mal-vec EXPR ...) #'(vector EXPR ...))
(define-macro (mal-map EXPR ...) #'(hash-map EXPR ...))
(define-macro (mal-special SPECIAL) #'SPECIAL)
(define-macro (mal-sym SYM) #'SYM)
(define-macro (mal-error ERR) #'ERR)
