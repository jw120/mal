#lang br

; Boot module for brmal language

; Reader module provdes read-syntax based on parse and tokenize
(module reader br
  (require brmal/parser
           brmal/tokenizer)
  (provide read-syntax)
  (define (read-syntax name port)
    (define the-parse-tree (parse (make-tokenizer port)))
    (strip-bindings
     #`(module brmal brmal/expander
         #,the-parse-tree))))
