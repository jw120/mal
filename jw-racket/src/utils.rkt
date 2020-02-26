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
          (if in-escape
              (let ([next-str (match ch
                                [#\n "\n"]
                                [#\" "\""]
                                [#\\ "\\"]
                                [x (raise-mal-read (format "Unknown escape ~a in remove-escapes" x))])])
                (values (string-append acc next-str) #f))
              (if (equal? ch #\\)
                  (values acc #t)
                  (values (string-append acc (string ch)) #f))))])
    (if still-in-escape
        (raise-mal-read "Escape unbalanced in remove-escapes")
        result)))
