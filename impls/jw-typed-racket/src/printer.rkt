#lang typed/racket

(require "types.rkt")

(provide pr_str)

(define (pr_str [val : Mal] [readable : Boolean]) : String
  (match val
    [i #:when (exact-integer? i) (number->string i)]
    [s #:when (string? s) (if readable
                              (string-append "\"" (add-escapes s) "\"")
                              s)]
    [s #:when (symbol? s) (symbol->string s)]
    [#t "true"]
    [#f "false"]
    [(mal-nil) "nil"]
    [(mal-list meta xs) (pr_sequence "(" ")" xs readable)]
    [(mal-vector _ v) (pr_sequence "[" "]" (vector->list v) readable)]
    [(mal-hash _ m) (pr_sequence "{" "}" (mal-hashmap->flat-list m) readable)]
    [(mal-function _ f) "#<function"]
    [_ (error "Unmatched in pr_str" val)]))

;; Helper function to print lists, vectors and hashes
(define (pr_sequence [opener : String] [closer : String] [elements : (Listof Mal)] [readable : Boolean])
  (define (pr_element [x : Mal]) : String
    (pr_str x readable))
  (string-append
   opener
   (string-join (map pr_element elements))
   closer))

(define (add-escapes [s : String]) : String
  (define translated-chars : (Listof String)
    (for/list ([ch (in-string s)])
      (match ch
        [#\newline "\\n"]
        [#\" "\\\""]
        [#\\ "\\\\"]
        [c (string c)])))
  (string-append* translated-chars))

(define (mal-hashmap->flat-list [m : (HashTable String Mal)]) : (Listof Mal)
  (define kv-list : (Listof (Pairof String Mal)) (hash->list m))
  (define (go [xs : (Listof (Pairof Mal Mal))]) : (Listof Mal)
    (if (null? xs)
        '()
        (cons (caar xs) (cons (cdar xs) (go (cdr xs))))))
  (go kv-list))


(module+ test
  (require typed/rackunit)

  (check-equal? (add-escapes "a\"b") "a\\\"b" "Escapes double-quote")
  (check-equal? (add-escapes "a\nb") "a\\nb" "Escapes newline")
  (check-equal? (add-escapes "a\\b") "a\\\\b" "Escapes backslash")
  
  (check-equal? (pr_str 23 true) "23")
  (check-equal? (pr_str "abc" true) "\"abc\"")
  (check-equal? (pr_str "def" false) "def")
  (check-equal? (pr_str 'xyz true) "xyz")
  (check-equal? (pr_str true true) "true")
  (check-equal? (pr_str false true) "false")
  (check-equal? (pr_str (mal-nil) true) "nil")
  (check-equal? (pr_str (mal-list (mal-nil) '(2 3)) true) "(2 3)")
  (check-equal? (pr_str (mal-vector (mal-nil) '#(4 5)) true) "[4 5]")
  (check-equal? (pr_str (mal-hash (mal-nil) '#hash(("a" . 2))) false) "{a 2}"))
  


