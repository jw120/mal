#lang br/quicklang

;; Version of our language that we can use with
;;
;; #lang brmal/parse-only
;;
;; that just parses the input

(require brmal/parser brmal/tokenizer)

(define (read-syntax path port)
  (define parse-tree (parse path (make-tokenizer port)))
  (strip-bindings
   #`(module mal-parser-mod brmal/parse-only
       #,parse-tree)))
(module+ reader (provide read-syntax))

(define-macro (parser-only-mb PARSE-TREE)
  #'(#%module-begin
     'PARSE-TREE))
(provide (rename-out [parser-only-mb #%module-begin]))
