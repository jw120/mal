#lang racket

(provide (contract-out
          [pr_str (-> any/c boolean? string?)]))

(require "exceptions.rkt" "utils.rkt")

(define (pr_str val print_readably)
  (cond
    [(string? val) (string-append "\"" (if print_readably (add-escapes val) val) "\"")]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]
    [(keyword? val) (string-append ":" (keyword->string val))]
    [(equal? val #t) "true"]
    [(equal? val #f) "false"]
    [(nil? val) "nil"]
    [(list? val) (pr_sequence "(" ")" val print_readably)]
    [(hash? val) (pr_sequence "{" "}" (flatten (hash->list val)) print_readably)]
    [(vector? val) (pr_sequence "[" "]" (vector->list val) print_readably)]
    [(procedure? val) "#<procedure>"]
    [else (raise-mal-fail (format "Unknown in print: ~a" val))]))

(define (pr_sequence opener closer elements print_readably)
    (string-append
        opener
        (string-join (map (lambda (x) (pr_str x print_readably)) elements))
        closer))
