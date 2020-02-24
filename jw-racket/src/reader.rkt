#lang racket

(provide (contract-out
          [read_string (-> string? any/c)]))

; top-level reading function
(define (read_string s)
  (read_form (new token-reader% [target-string s])))

; regexp for our tokens
(define mal-regexp #px"[\\s,]*(~@|[]\\[{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)")

; class for stateful reader object
(define token-reader%
  (class object%
    (init target-string)
    (define current-pos 0)
    (define current-string target-string)
    (super-new)
    (define/public (next-token)
      (match (regexp-match-positions mal-regexp current-string current-pos)
        [(list (cons whole-start whole-end) (cons group-start group-end))
         (set! current-pos group-end)
         ;         (display "next ")
         ;         (println (substring current-string group-start group-end))
         (substring current-string group-start group-end)]))
    (define/public (peek-token)
      (match (regexp-match-positions mal-regexp current-string current-pos)
        [(list (cons whole-start whole-end) (cons group-start group-end))
         ;         (display "peek ")
         ;         (println (substring current-string group-start group-end))
         (substring current-string group-start group-end)]))))

(define (read_form r)
  (match (send r peek-token)
    ["" ""]
    [")" ")"]
    ["(" (read_list r)]
    [_ (read_atom r)]))

(define (read_list r)
  (unless (equal? (send r next-token) "(")
    (error "no opening paren in read_list"))
  (define last-form "")
  (define acc-list (for/list
                       ([form (in-producer (lambda ()
                                             (set! last-form (read_form r))
                                             last-form))]
                        #:break (or (equal? form ")") (equal? form "")))
                    form))
  (unless (equal? last-form ")")
    (error "EOF before closing paren in read_list"))
  acc-list)

(define (read_atom r)
  (define t (send r next-token))
  (match t
    ["" (error "Unexpected EOF in read_atom")]
    [(regexp #px"^[[:digit:]]+$") (string->number t)]
    [(regexp #rx"^\\\".*\\\"$") (substring t 1 (- (string-length t) 1))]
    [_ (string->symbol t)]))
  
