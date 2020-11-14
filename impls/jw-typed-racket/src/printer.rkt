#lang typed/racket

(require "types.rkt" "utils.rkt")

(provide pr_str)

(define (pr_str [val : Mal] [readable : Boolean]) : String
  (match val
    [(? exact-integer? i) (number->string i)]
    [(? string? s) (if readable
                       (string-append "\"" (add-escapes s) "\"")
                       s)]
    [(mal-nil) "nil"]
    [(? symbol? s) (symbol->string s)]
    [#t "true"]
    [#f "false"]
    [(box b) (string-join (list "(atom " (pr_str b readable) ")") "")]
    [(mal-keyword s) (string-append ":" s)]
    [(mal-list xs) (pr_sequence "(" ")" xs readable)]
    [(mal-vector v) (pr_sequence "[" "]" (vector->list v) readable)]
    [(mal-hash m) (pr_sequence "{" "}" (mal-hashmap->flat-list m) readable)]
    [(mal-function _) "#<function>"]
    [(mal-macro _) "#<macro>"]
    [(? void? _) "#<void>"]
    [_ (error "Unmatched in pr_str" val)]))

;; Helper function to print lists, vectors and hashes
(define (pr_sequence [opener : String] [closer : String] [elements : (Listof Mal)] [readable : Boolean])
  (define (pr_element [x : Mal]) : String
    (pr_str x readable))
  (string-append
   opener
   (string-join (map pr_element elements))
   closer))

(module+ test
  (require typed/rackunit)
  
  (check-equal? (pr_str 23 true) "23")
  (check-equal? (pr_str "abc" true) "\"abc\"")
  (check-equal? (pr_str "def" false) "def")
  (check-equal? (pr_str 'xyz true) "xyz")
  (check-equal? (pr_str true true) "true")
  (check-equal? (pr_str false true) "false")
  (check-equal? (pr_str (mal-nil) true) "nil")
  (check-equal? (pr_str (mal-list '(2 3)) true) "(2 3)")
  (check-equal? (pr_str (mal-vector '#(4 5)) true) "[4 5]")
  (check-equal? (pr_str (mal-hash '#hash(("a" . 2))) false) "{a 2}")
  (check-equal? (pr_str (box 3) true) "(atom 3)"))
  


