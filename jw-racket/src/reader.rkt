#lang racket

(provide (contract-out
          [read_string (-> string? any/c)]))

(require "exceptions.rkt")

;; top-level reading function which we export. Sets up reader and hands over to read_form
(define (read_string s)
  (read_form (new token-reader% [target-string s])))

;; regexp for our tokens
(define mal-regexp #px"[\\s,]*(~@|[]\\[{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)")

;; class for stateful reader object
(define token-reader%
  (class object%
    (init target-string)
    (define current-pos 0)
    (define current-string target-string)
    (super-new)
    (define/private (get-token advance-pos?)
      (match (regexp-match-positions mal-regexp current-string current-pos)
        [(list (cons whole-start whole-end) (cons group-start group-end))
         (when advance-pos? (set! current-pos group-end))
         (substring current-string group-start group-end)]))
    (define/public (next-token)
      (get-token #t))
    (define/public (peek-token)
      (get-token #f))))

;; top-level internal function to read from our reader
(define (read_form r)
  (match (send r peek-token)
    ["" (raise-mal-empty)]
    ["(" (read_list r)]
    ["[" (read_vector r)]
    [_ (read_atom r)]))

;; read a list
(define (read_list r)
  (unless (equal? (send r next-token) "(")
    (raise-mal-fail "no opening paren in read_list"))
  (read-forms-until ")" r))

;; read a vector
(define (read_vector r)
  (unless (equal? (send r next-token) "[")
    (raise-mal-fail "no opening bracker in read_vector"))
  (vector->immutable-vector
   (list->vector
    (read-forms-until "]" r))))

;; read a list of forms until (but not including) the given token
(define (read-forms-until end-token r)
  (with-handlers ([exn:mal:empty? (λ (exn) (raise-mal-read "EOF found reading list or vector"))])
    (let ([next-form (read_form r)])
      (cond
        [(equal? next-form end-token) '()]
        [else (cons next-form (read-forms-until end-token r))]))))

;; read an atom
(define (read_atom r)
  (define t (send r next-token))
  (match t
    ["" (raise-mal-fail "Unexpected EOF in read_atom")]
    ["true" #t]
    ["false" #f]
    ["nil" 'nil]
    ["'" (list 'quote (read_form r))]
    ["`" (list 'quasiquote (read_form r))]
    ["~" (list 'unquote (read_form r))]
    ["^"
     (let* ([x (read_form r)]
            [y (read_form r)])
       (list 'with-meta y x))]
    ["@" (list 'deref (read_form r))]
    ["~@" (list 'splice-unquote (read_form r))]
    [(regexp #px"^[{}()\\[\\]]") t] ; tokens for delimiters (returned as strings)
    [(regexp #px"^-?[[:digit:]]+$") (string->number t)] ; number
    [(regexp #rx"^\\\".*\\\"$") (substring t 1 (- (string-length t) 1))] ; quoted string
    [(regexp #rx"^\\\".*$") (raise-mal-read "EOF reading string")]
    [_ (string->symbol t)])) ; anything else is a symbol
