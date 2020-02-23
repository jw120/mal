#lang racket

(provide (contract-out
          [read_string (-> string? any)]))

(define mal-regexp #px"[\\s,]*(~@|[]\\[{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)")
(define current-input-string " (+ 2 3)")
(define current-input-pos 0)

(define (match-token)
  (regexp-match-positions mal-regexp current-input-string current-input-pos))
  
(define (next-token)
  (match (regexp-match-positions mal-regexp current-input-string current-input-pos)
    [(list (cons whole-start whole-end) (cons group-start group-end))
     (set! current-input-pos group-end)
     (substring current-input-string group-start group-end)]))
(define (peek-token)
  (match (regexp-match-positions mal-regexp current-input-string current-input-pos)
    [(list (cons whole-start whole-end) (cons group-start group-end))
     (substring current-input-string group-start group-end)]))
    
(define (read_string s)
  (set! current-input-string s)
  (set! current-input-pos 0)
  (read_form))

(define (read_form)
  (match (peek-token)
    ["(" (read_list)]
    ["" ""]
    [_ (read_atom)]))

(define (read_list)
  "list")
(define (read_atom)
  "atom")

(read_string "(+ 2)")
(read_string "23")
(read_string "()")
(read_string "")


