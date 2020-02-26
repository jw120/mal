#lang racket

(provide (contract-out
          [pr_str (-> any/c string?)]))

(require "exceptions.rkt")

(define (pr_str val)
  (cond
    [(string? val) (string-append "\"" val "\"")]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]
    [(keyword? val) (string-append ":" (keyword->string val))]
    [(equal? val #t) "true"]
    [(equal? val #f) "false"]
    [(list? val)
     (string-append "(" (string-join (map pr_str val)) ")")]
    [(hash? val)
     (string-append "{" (string-join (map pr_str (flatten (hash->list val)))) "}")]
    [(vector? val)
     (string-append "[" (string-join (map pr_str (vector->list val))) "]")]
    [else (raise-mal-fail (format "Unknown in print: ~a" val))]))
    