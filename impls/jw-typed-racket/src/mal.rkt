#lang typed/racket

(require "core.rkt" "env.rkt" "eval.rkt" "printer.rkt" "reader.rkt" "types.rkt")
(require/typed readline/readline
               [readline (-> String (U String EOF))]
               [add-history (-> String Void)])

(define (READ [s : String]) : Mal
  (read_str s))

(define (PRINT [x : Mal]) : String
  (pr_str x true))

(define (rep [s : String] [env : mal-env]) : (U String Void)
  (let ([result (EVAL (READ s) env)])
    (if (void? result)
        (void)
        (PRINT result))))

(define repl_env : mal-env
  (env-new core_ns #f))

(env-set! repl_env
          'eval
          (mal-function #f #f
           (λ ([args : (Listof Mal)])
             (match args
               [(list ast) (EVAL ast repl_env)]
               [_ (raise-mal "Bad arguments to eval")]))))
(env-set! repl_env '*ARGV* (mal-list #f #f '()))

(define repl-startup-code : (Listof String)
  (list
   "(def! not (fn* (a) (if a false true)))"
   "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
   (string-append
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) "
    "(if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) "
    "(cons 'cond (rest (rest xs)))))))")))
(for ([s repl-startup-code])
  (EVAL (READ s) repl_env))

(define (repl) : Void
  (define user-input : (U String EOF) (readline "user> "))
  (cond [(eq? user-input eof)
         (displayln "")] ; On Ctrl-D, clean up by printing a newline
        [(and (not (void? user-input)) (non-empty-string? user-input))
         (add-history user-input)
         (with-handlers
             ([exn:mal?
               (λ ([exn : exn:mal])
                 (printf "Exception: ~a\n" (pr_str (exn:mal-thrown-value exn) #f)))])
           (let ([output : (U String Void) (rep user-input repl_env)])
             (unless (void? output)
               (displayln output))))
         (repl)]
        [else
         (repl)]))

; Either start the repl or load the given file
(cond
  [(equal? 0 (vector-length (current-command-line-arguments)))
   (rep "(println (str \"Mal [\" *host-language* \"]\"))" repl_env)
   (repl)]
  [else
   (env-set! repl_env '*ARGV* (mal-list #f #f (vector->list (vector-drop (current-command-line-arguments) 1))))
   (rep (format "(load-file ~s)" (vector-ref (current-command-line-arguments) 0)) repl_env)
   (void)]) ; void to avoid returning the value from rep
