#lang br

(require brmal/core
         brmal/exceptions
         brmal/parser
         brmal/printer
         brmal/tokenizer
         brag/support
         readline/readline)

(define mal-namespace (module->namespace 'brmal/core))

(define (repl)
  (define input-string (readline "user> "))
  (cond
    [(eq? input-string eof)
     (displayln "")] ; On Ctrl-D, clean up by printing a newline
    [else
     (define input-tokens (apply-lexer mal-lexer input-string))
;;     (printf "tokens: ~a\n" input-tokens)
     (define input-parsed (parse-to-datum (make-tokenizer (open-input-string input-string))))
;;     (printf "parsed: ~a\n" input-parsed)
     (with-handlers
       ([exn:mal:read? (λ (exn) (displayln (exn-message exn)))]
       [exn:fail? (λ (exn) (displayln (exn-message exn)))])
       (define input-evaluated (eval input-parsed mal-namespace))
       (displayln (pr_str input-evaluated #t)))
     (repl)]))
(repl)
