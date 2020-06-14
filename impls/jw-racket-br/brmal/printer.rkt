#lang racket/base

(require brmal/core
         racket/contract/base
         racket/list
         racket/match
         racket/string)

(provide (contract-out
          [pr_str (-> any/c boolean? string?)]))

(define (pr_str val print_readably)
  (cond
    [(and (string? val) (string-prefix? val keyword-prefix))
     (string-append ":" (substring val 1))]
    [(string? val) (if print_readably
                       (string-append "\"" (add-escapes val) "\"")
                       val)]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]
    [(equal? val #t) "true"]
    [(equal? val #f) "false"]
;;    [(nil? val) "nil"]
    [(list? val) (pr_sequence "(" ")" val print_readably)]
    [(hash? val) (pr_sequence "{" "}" (flatten (hash->list val)) print_readably)]
    [(vector? val) (pr_sequence "[" "]" (vector->list val) print_readably)]
;;    [(box? val) (string-join (list "(atom " (pr_str (unbox val) print_readably) ")") "")]
    [(procedure? val) "#<procedure>"]
;;    [(func? val) "#<function>"]
    [else (raise (format "Unknown in print: ~a" val))]))

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
