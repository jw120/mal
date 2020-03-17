#lang br

(require brmal/exceptions
         brmal/parser
         brmal/printer
         brmal/tokenizer
         readline/readline)

(define (repl)
  (define input-string (readline "user> "))
  (cond
    [(eq? input-string eof)
     (displayln "")] ; On Ctrl-D, clean up by printing a newline
    [else
     (define input-parsed (parse-to-datum (make-tokenizer (open-input-string input-string))))
     (with-handlers
       ([exn:mal:read? (λ (exn) (displayln (exn-message exn)))]
       [exn:fail? (λ (exn) (displayln (exn-message exn)))])
       (displayln (pr_str input-parsed #t)))
     (repl)]))
(repl)
