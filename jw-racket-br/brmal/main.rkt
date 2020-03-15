#lang racket/base

; Boot module for brmal language

(require brmal/core)

(provide (all-from-out brmal/core)
         #%app
         #%top-interaction
         #%top
         #%app
         banner
         #%datum)

(define-syntax-rule (my-module-begin SEXP)
  #'(#%module-begin ;; from brmal
     SEXP))

(current-prompt-read
 (let ([old-prompt-read (current-prompt-read)])
   (lambda () (display "user") (flush-output) (old-prompt-read))))

(define (banner)
  "MAL\n")

(displayln "In brmal/main")

;(define (margrave-repl-prompt-read)
;  (display "Margrave> ") (flush-output)
;  (let ([in (current-input-port)])
;    ((current-read-interaction) (object-name in) in)))

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
