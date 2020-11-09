#lang racket/base

(require racket/contract/base
         readline/readline
         "exceptions.rkt"
         "printer.rkt")

(provide (contract-out
          [repl (-> (-> string? string?) void?)]))

;; Implements our repl given the read-eval-print function
(define (repl rep-function)
  (define s (readline "user> "))
  (if (eq? s eof)
      (displayln "") ; On Ctrl-D, clean up by printing a newline
      (begin
        (cond
          [(> (string-length s) 0)
           (add-history s)
           (with-handlers
               (;;;[exn:break? (λ (exn) (displayln "Break") (exit))] ; Does not seem to work
                [exn:mal:empty? void]
                [exn:mal:fail? (λ (exn) (raise exn))]
                [exn:mal:throw? (λ (exn) (printf "Exception: ~a\n" (pr_str (exn:mal:throw-thrown-value exn) #t)))]
                [exn:mal? (λ (exn) (displayln (exn-message exn)))])
             (displayln (rep-function s)))])
        (repl rep-function))))
