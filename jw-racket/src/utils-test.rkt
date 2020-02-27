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

   (check-equal? (list-or-vector? '(1 2 3)) #t "list-or-vector list")
   (check-equal? (list-or-vector? '()) #t "list-or-vector empty list")
   (check-equal? (list-or-vector? '#[1 2 3]) #t "list-or-vector vector")
   (check-equal? (list-or-vector? "ab") #f "list-or-vector empty string")
   (check-equal? (list-or-vector? 2) #f "list-or-vector number")
   (check-equal? (list-or-vector->list '(1 2 3)) '(1 2 3) "list-or-vector->list list")
   (check-equal? (list-or-vector->list '()) '() "list-or-vector->list empty list")
   (check-equal? (list-or-vector->list '#[1 2 3]) '(1 2 3) "list-or-vector->list vector")

   
   ))
