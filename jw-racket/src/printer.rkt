#lang racket

(provide (contract-out
          [pr_str (-> any/c string?)]))

(define (pr_str val)
  (cond
    [(string? val) (string-append "\"" val "\"")]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]
    [(list? val) (string-append "(" (string-join (map pr_str val)) ")")]
    [else "Unknown"]))
