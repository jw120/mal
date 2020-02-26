#lang racket

(provide (contract-out
          [read_string (-> string? any/c)]))

(require "exceptions.rkt" "utils.rkt")

;; top-level reading function which we export. Sets up reader and hands over to read-form
(define (read_string s)
  (read-form (new token-reader% [target-string s])))


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
(define (read-form r)
  (match (send r peek-token)
    ["" (raise-mal-empty)]
    ["(" (read-list r)]
    ["[" (read-vector r)]
    ["{" (read-hash-map r)]
    [_ (read-atom r)]))

;; read a list
(define (read-list r)
  (unless (equal? (send r next-token) "(")
    (raise-mal-fail "no opening paren in read-list"))
  (read-forms-until ")" r))

;; read a vector
(define (read-vector r)
  (unless (equal? (send r next-token) "[")
    (raise-mal-fail "no opening bracker in read-vector"))
  (vector->immutable-vector
   (list->vector
    (read-forms-until "]" r))))

;; read a hash map
(define (read-hash-map r)
  (unless (equal? (send r next-token) "{")
    (raise-mal-fail "no opening brace in read-hash-map"))
  (apply hash (read-forms-until "}" r)))

;; read a list of forms until (but not including) the given token
(define (read-forms-until end-token r)
  (with-handlers ([exn:mal:empty? (λ (exn) (raise-mal-read "EOF found reading list or vector"))])
    (let ([next-form (read-form r)])
      (cond
        [(equal? next-form end-token) '()]
        [else (cons next-form (read-forms-until end-token r))]))))

;; read an atom
(define (read-atom r)
  (define t (send r next-token))
  (match t
    ["" (raise-mal-fail "Unexpected EOF in read-atom")]
    ["true" #t]
    ["false" #f]
    ["nil" 'nil]
    ["'" (list 'quote (read-form r))]
    ["`" (list 'quasiquote (read-form r))]
    ["~" (list 'unquote (read-form r))]
    ["^"
     (let* ([x (read-form r)]
            [y (read-form r)])
       (list 'with-meta y x))]
    ["@" (list 'deref (read-form r))]
    ["~@" (list 'splice-unquote (read-form r))]
    [(regexp #px"^[{}()\\[\\]]") t] ; tokens for delimiters (returned as strings)
    [(regexp #px"^-?[[:digit:]]+$") (string->number t)] ; number
    [(regexp #rx"^\\\".*\\\"$") (remove-escapes (substring t 1 (- (string-length t) 1)))] ; quoted string
    [(regexp #rx"^\\\".*$") (raise-mal-read "EOF reading string")]
    [(regexp #rx"^:.+$")  (string->keyword (substring t 1))] ; keyword
    [_ (string->symbol t)])) ; anything else is a symbol

;; regexp for our tokens (at end of file to avoid confusing our editors highlighting)
(define mal-regexp #px"[\\s,]*(~@|[]\\[{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)")