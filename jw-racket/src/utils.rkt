#lang racket

(provide nil
         (contract-out
          [nil? (-> any/c boolean?)]
          [add-escapes (-> string? string?)]
          [remove-escapes (-> string? string?)]
          [list-or-vector? (-> any/c boolean?)]
          [list-or-vector->list (-> list-or-vector? list?)]))

(require "exceptions.rkt")

;; Racket has no nil (just #f and '() which is null), we create one here
(define nil (string->uninterned-symbol "nil"))
(define (nil? x) (eq? x nil))

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
