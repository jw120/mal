#lang racket/base

; Boot module for brmal language

(require brmal/core)

(provide (all-from-out brmal/core)
         #%app
         #%top-interaction
         #%top
         #%app
         #%datum)

(define-syntax-rule (my-module-begin SEXP)
  #'(#%module-begin ;; from brmal
     SEXP))

; Reader module provdes read-syntax based on parse and tokenize
(module reader br
  (require brmal/parser
           brmal/tokenizer)
  (provide read-syntax)
  (define (read-syntax name port)
    (define the-parse-tree (parse (make-tokenizer port)))
    (strip-bindings
     #`(module brmal brmal/expander
         #,the-parse-tree))))
