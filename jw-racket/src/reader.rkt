#lang racket

(provide (contract-out
          [read_string (-> string? any/c)]))

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
    ["" ""]
    ["(" (read_list r)]
    ["[" (read_vector r)]
    [_ (read_atom r)]))

;; read a list
(define (read_list r)
  (unless (equal? (send r next-token) "(")
    (error "no opening paren in read_list"))
  (read-forms-until ")" r))

;; read a vector
(define (read_vector r)
  (unless (equal? (send r next-token) "[")
    (error "no opening bracker in read_vector"))
  (vector->immutable-vector
   (list->vector
    (read-forms-until "]" r))))

;; read a list of forms until (but not including) the given token
(define (read-forms-until end-token r)
  (let ([next-form (read_form r)])
    (cond
      [(equal? next-form end-token) '()]
      [(equal? next-form "") (error "EOF in read-forms-until")]
      [else (cons next-form (read-forms-until end-token r))])))

;; read an atom
(define (read_atom r)
  (define t (send r next-token))
  (match t
    ["" (error "Unexpected EOF in read_atom")]
    [(regexp #px"^[\\[\\]{}()'`~^@]") t] ; single-character token (returned as a string)
    ["~@" t] ; two-charcter token (as a string)
    ["true" #t]
    ["false" #f]
    ["nil" 'nil]
    [(regexp #px"^[[:digit:]]+$") (string->number t)] ; number
    [(regexp #rx"^\\\".*\\\"$") (substring t 1 (- (string-length t) 1))] ; quoted string
    [_ (string->symbol t)])) ; anything else is a symbol
  