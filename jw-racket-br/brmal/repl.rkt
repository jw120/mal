#lang br

(require readline/readline brmal/parser brmal/tokenizer brmal/core)

(displayln "repl-start")
;(define (repl)
(define s (readline "user> "))
(define the-parse-tree (parse-to-datum (make-tokenizer (open-input-string s))))
(display "repl-parsed: ")
(displayln the-parse-tree)
;(define expanded-form (strip-bindings
;                      #`(module brmal brmal/expander
;                          #,the-parse-tree)))
(define stripped-form (strip-bindings the-parse-tree))
(display "repl-stripped: ")
(displayln stripped-form)
(define ns (module->namespace 'brmal/core))
(define val (eval stripped-form ns))
(display "repl-evaled: ")
(displayln val)
;(repl)
;  (if (eq? s eof)
;      (displayln "") ; On Ctrl-D, clean up by printing a newline
;      (begin
;        (define the-parse-tree (parse (make-tokenizer (open-input-string s))))
;        (define expanded-form (strip-bindings
;                      #`(module brmal brmal/expander
;                          #,the-parse-tree)))
;        (print (eval expanded-form))
;        (repl))))
