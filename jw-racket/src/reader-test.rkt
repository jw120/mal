#lang racket/base
 
(require rackunit
         "exceptions.rkt"
         "reader.rkt")

(require rackunit/text-ui)

(provide reader-tests)

(define reader-tests
  (test-suite
   "Tests for reader.rkt"

   ; empty input gives exceptions
   (check-exn exn:mal:empty? (λ () (read_string "")) "Empty string")
   (check-exn exn:mal:empty? (λ () (read_string "  ")) "White space")

   ; sequences
   (check-equal? (read_string "(1 2 3)") '(1 2 3) "List")
   (check-equal? (read_string "()") '() "Empty list")
   (check-equal? (read_string "[1 2 3]") #(1 2 3) "Vector")
   (check-equal? (read_string "[1,2,4]") #(1 2 4) "Vector with commas")
   (check-equal? (read_string "[]") #() "Empty vector")
   (check-equal? (read_string "(1 (2 3))") '(1 (2 3)) "List of lists")
   (check-equal? (read_string "(()())") '(()()) "Empty list of empty lists")
   (check-equal? (read_string "{ \"a\" 1 \"bb\" 2}") #hash(("a" . 1) ("bb" . 2)) "Hash map")
   (check-equal? (read_string "(()())") '(()()) "Empty hash map")
   (check-exn exn:mal:read? (λ () (read_string "(1 2")) "Unterminated list")
   (check-exn exn:mal:read? (λ () (read_string "(1 2]")) "Wrongly terminated list")
   (check-exn exn:mal:read? (λ () (read_string "[1 2")) "Unterminated vector")
   (check-exn exn:mal:read? (λ () (read_string "{1 2")) "Unterminated hash map")
   
   ; strings
   (check-equal? (read_string "\"pq\"") "pq" "String")
   (check-equal? (read_string "\"\"") "" "Empty string")
   (check-exn exn:mal:read? (λ () (read_string "\"abc")) "Unterminated string")

   ; numbers
   (check-equal? (read_string "123") 123 "Number")
   (check-equal? (read_string "-45") -45 "Negative number")

   ; special symbols
   (check-equal? (read_string "'pqr") ''pqr "Quote")
   (check-equal? (read_string "`pqr") '`pqr "Quasiquote")
   (check-equal? (read_string "~pqr") ',pqr "Unquote")
   (check-equal? (read_string "^pqr abc") '(with-meta abc pqr) "With-meta")
   (check-equal? (read_string "@pqr") '(deref pqr) "Deref")
   (check-equal? (read_string "~@pqr") '(splice-unquote pqr) "Splice-unquote")

   ; keyword
   (check-equal? (read_string ":pqr") '#:pqr "Keyword")
   
   ; symbol
   (check-equal? (read_string "pqr") 'pqr "Symbol")
   (check-equal? (read_string "true") #t "Symbol")
   (check-equal? (read_string "false") #f "Symbol")
   (check-equal? (read_string "nil") 'nil "Symbol")
   ))
