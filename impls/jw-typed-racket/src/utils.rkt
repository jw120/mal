#lang racket

(provide nil
         (contract-out
          [struct func ((is-macro? boolean?) (closure procedure?) (meta any/c))]
          [nil? (-> any/c boolean?)]
          [mal-symbol? (-> any/c boolean?)]
          [add-escapes (-> string? string?)]
          [remove-escapes (-> string? string?)]
          [list-or-vector? (-> any/c boolean?)]
          [list-or-vector->list (-> list-or-vector? list?)]))

(require "exceptions.rkt")

;; Define our function type here
(struct func (is-macro? closure meta) #:transparent)

;; Racket has no nil (just #f and '() which is null), we create one here
;; As nil is not a mal-symbol we create a new predicate to detect a mal symbol
(define nil (string->uninterned-symbol "nil"))
(define (nil? x) (eq? x nil))
(define (mal-symbol? x) (and (symbol? x) (not (nil? x))))

(define (add-escapes s)
  (list->string
   (flatten
    (for/list ([ch (in-string s)])
      (match ch
        [#\newline '(#\\ #\n)]
        [#\" '(#\\ #\")]
        [#\\ '(#\\ #\\)]
        [c c])))))

(define (remove-escapes s)
  (let-values
      ([(result still-in-escape)
        (for/fold ([acc ""]
                   [in-escape #f])
                  ([ch (in-string s)])
          (match (list in-escape ch)
            [(list #t #\n) (values (string-append acc "\n") #f)]
            [(list #t #\") (values (string-append acc "\"") #f)]
            [(list #t #\\) (values (string-append acc "\\") #f)]
            [(list #t x) (raise-mal-read (format "Unknown escape ~a in remove-escapes" ch))]
            [(list #f #\\) (values acc #t)]
            [(list #f x) (values (string-append acc (string ch)) #f)]))])
    (when still-in-escape (raise-mal-read "Escape unbalanced in remove-escapes"))
    result))

; Helper function: is the element a list or vector
(define (list-or-vector? x)
  (or (list? x) (vector? x)))

; Helper function: convert list or vector to a list
(define (list-or-vector->list x)
  (cond
    [(list? x) x]
    [(vector? x) (vector->list x)]
    [else (raise-mal-eval "Bad input to list-or-vector->list")]))


(module+ test
  (require rackunit)

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
)
