#lang typed/racket/base

;(require typed/racket/list
;         typed/racket/string)
;;         "exceptions.rkt"
;;         "utils.rkt")

(provide pr_str)

(define (pr_str [val : Mal] [print_readably : Boolean]) : String
  (cond
    [(string? val) (if print_readably
                       (string-append "\"" (add-escapes val) "\"")
                       val)]
    [(number? val) (number->string val)]
    [(mal-symbol? val) (symbol->string val)]
    [(keyword? val) (string-append ":" (keyword->string val))]
    [(equal? val #t) "true"]
    [(equal? val #f) "false"]
    [(nil? val) "nil"]
    [(list? val) (pr_sequence "(" ")" val print_readably)]
    [(hash? val) (pr_sequence "{" "}" (flatten (hash->list val)) print_readably)]
    [(vector? val) (pr_sequence "[" "]" (vector->list val) print_readably)]
    [(box? val) (string-join (list "(atom " (pr_str (unbox val) print_readably) ")") "")]
    [(procedure? val) "#<procedure>"]
    [(func? val) "#<function>"]
    [else (raise-mal-fail (format "Unknown in print: ~a" val))]))

(define (pr_sequence opener closer elements print_readably)
    (string-append
        opener
        (string-join (map (lambda (x) (pr_str x print_readably)) elements))
        closer))

(define (add-escapes s)
  (list->string
   (flatten
    (for/list ([ch (in-string s)])
      (match ch
        [#\newline '(#\\ #\n)]
        [#\" '(#\\ #\")]
        [#\\ '(#\\ #\\)]
        [c c])))))
