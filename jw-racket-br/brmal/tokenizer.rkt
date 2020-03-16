#lang racket/base

; Simple lexer based on the regexp given in the mal instructions

(provide make-tokenizer)

(require brag/support)

(define-lex-abbrev other-special-char (char-set "`'~^@"))
(define-lex-abbrev ordinary-char (:~ (char-set "[]{}()`'~^@,; \n")))
(define-lex-abbrev escaped-char (:: "\\" any-char))
(define-lex-abbrev not-double-quote (:~ (char-set "\"")))

(define mal-lexer
  (lexer
   [(:+ (:or whitespace "," "\n"))
    (token 'WHITE lexeme #:skip? #t)]
   ["(" (token 'OPEN-PAREN lexeme)]
   [")" (token 'CLOSE-PAREN lexeme)]
   ["[" (token 'OPEN-SQUARE lexeme)]
   ["]" (token 'CLOSE-SQUARE lexeme)]
   ["{" (token 'OPEN-CURLY lexeme)]
   ["}" (token 'CLOSE-CURLY lexeme)]
   [(:or "~@" other-special-char)
    (token 'SPECIAL lexeme)]
   [(:: (:? "-") (:+ (char-set "01234567890")))
    (token 'INTEGER (string->number lexeme))]
   [(from/to ";" "\n")
    (token 'COMMENT (trim-ends ";" lexeme "\n") #:skip? #t)]
   [(:: "\"" (:* (:or escaped-char not-double-quote)) "\"")
    (token 'STRING (trim-ends "\"" lexeme "\""))]
   [(:: "\"" (:* (:or escaped-char not-double-quote)))
    (token 'UNTERMINATED-STRING lexeme)]
   [(:: ":" (:+ ordinary-char))
    (token 'KEYWORD (substring lexeme 1))]
   [(:+ ordinary-char)
    (token 'SYMBOL (string->symbol lexeme))]))

(define (make-tokenizer port)
  (define (next-token)
    (mal-lexer port))
  next-token)


(module+ test
  (require rackunit)
  (define (mal-lex str) (apply-port-proc mal-lexer str))
  (define (mal-lex-tv str)
    (map
     (λ (t) (list (token-struct-type t) (token-struct-val t)))
     (mal-lex str))))

(module+ test
  (check-equal? (mal-lex-tv "  ") '((WHITE "  ")))
  (check-equal? (mal-lex-tv "~@") '((SPECIAL "~@")))
  (check-equal? (mal-lex-tv "^") '((SPECIAL "^")))
  (check-equal? (mal-lex-tv "12") '((INTEGER 12)))
  (check-equal? (mal-lex-tv "-34") '((INTEGER -34)))
  (check-equal? (mal-lex-tv ";comment\n") '((COMMENT "comment")))
  (check-equal? (mal-lex-tv "abc") '((SYMBOL abc)))
  (check-equal? (mal-lex-tv ":abc") '((KEYWORD abc)))
  (check-equal? (mal-lex-tv "\"xyz\"") '((STRING "xyz")))
  (check-equal? (mal-lex-tv "\"p\\\"q\"") '((STRING "p\\\"q")))
  (check-equal? (mal-lex-tv "\"sdf") '((UNTERMINATED-STRING "\"sdf")))

  (check-equal? (mal-lex-tv "  99   ") '((WHITE "  ") (INTEGER 99) (WHITE "   ")))
  (check-equal? (mal-lex-tv "22\n\n") '((INTEGER 22) (WHITE "\n\n")))
  (check-equal? (mal-lex-tv ";a\n34;b\n;c\n") '((COMMENT "a") (INTEGER 34) (COMMENT "b") (COMMENT "c")))

  (check-equal? (mal-lex-tv "{:a (+ 7 8)}") '())
  )
