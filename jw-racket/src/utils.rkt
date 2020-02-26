#lang racket

(provide (contract-out
          [add-escapes (-> string? string?)]
          [remove-escapes (-> string? string?)]))

(require "exceptions.rkt")

(define (add-escapes s)
  (list->string
   (flatten
    (for/list ([ch (in-string s)])
      (match ch
        [#\newline '(#\\ #\n)]
        [#\" '(#\\ #\")]
        [#\\ '(#\\ #\\)]
        [c c])))))

(define (remove-escapes s)
  (let-values
      ([(result still-in-escape)
        (for/fold ([acc ""]
                   [in-escape #f])
                  ([ch (in-string s)])
          (match (list in-escape ch)
            [(list #t #\n) (values (string-append acc "\n") #f)]
            [(list #t #\") (values (string-append acc "\"") #f)]
            [(list #t #\\) (values (string-append acc "\\") #f)]
            [(list #t x) (raise-mal-read (format "Unknown escape ~a in remove-escapes" ch))]
            [(list #f #\\) (values acc #t)]
            [(list #f x) (values (string-append acc (string ch)) #f)]))])
    (when still-in-escape (raise-mal-read "Escape unbalanced in remove-escapes"))
    result))
  