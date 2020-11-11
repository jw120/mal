#lang typed/racket

(require "core.rkt" "env.rkt" "printer.rkt" "reader.rkt" "types.rkt")
(require/typed readline/readline
               [readline (-> String (U String EOF))]
               [add-history (-> String Void)])

(define (READ [s : String]) : Mal
      (read_string s))

(define (EVAL [x : Mal] [env : mal-env]) : Mal
  x)

(define (PRINT [x : Mal]) : String
  (pr_str x true))

(define (rep [s : String] [env : mal-env]) : String
  (PRINT (EVAL (READ s) env)))

(define repl_env : mal-env
  (env-new core_ns #f))

(define (repl) : Void
  (define user-input : (U String EOF) (readline "user> "))
  (cond [(eq? user-input eof)
         (displayln "")] ; On Ctrl-D, clean up by printing a newline
        [(non-empty-string? user-input)
         (add-history user-input)
         (with-handlers
             ([exn:mal-empty?
               (λ ([exn : exn:mal-empty])
                 void)] ; do nothing if signalled no input
              [exn:mal?
               (λ ([exn : exn:mal])
                 (printf "Exception: ~a\n" (pr_str (exn:mal-thrown-value exn) #f)))])
           (displayln (rep user-input repl_env)))
         (repl)]
        [else
         (repl)]))

(repl)