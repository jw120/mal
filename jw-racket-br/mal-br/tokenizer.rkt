#lang br
(require brag/support)

(provide tokenize)

(define mal-lexer
  (lexer-src-pos
