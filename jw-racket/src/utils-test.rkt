#lang racket/base
 
(require rackunit
         "exceptions.rkt"
         "utils.rkt")

(provide utils-tests)

(define utils-tests
  (test-suite
   "Tests for utils.rkt"

   (check-equal? (add-escapes "a\"b") "a\\\"b" "Escapes double-quote")
   (check-equal? (add-escapes "a\nb") "a\\nb" "Escapes newline")
   (check-equal? (add-escapes "a\\b") "a\\\\b" "Escapes backslash")

   (check-equal? (remove-escapes "a\\\"b") "a\"b" "De-escapes double-quote")
   (check-equal? (remove-escapes "a\\nb") "a\nb" "De-escapes newline")
   (check-equal? (remove-escapes "a\\\\b") "a\\b" "De-escapes backslash")
   
   (check-exn exn:mal:read? (λ () (remove-escapes "\\")) "De-escape fails on naked backslash")
   (check-exn exn:mal:read? (λ () (remove-escapes "\\Q")) "De-escape fails on unknown backslash")
  ))
