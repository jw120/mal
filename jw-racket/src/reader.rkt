#lang racket/base

(require racket/class
         racket/contract/base
         racket/match
         "exceptions.rkt"
         "utils.rkt")

(provide (contract-out
          [read_string (-> string? any/c)]))

; We use the symbols eof (defined by system) and white-space (defined here)
; to match inside this file (end of input and spaces/comments respectively)
; They are converted to the empty exception in read_string
(define white-space (string->uninterned-symbol "white-space"))

;; top-level reading function which we export. Sets up reader and hands over to read-form
;; If no input (or just space/comment raise the empty exception
(define (read_string s)
;  (read-form (new token-reader% [target-string s])))
  (let ([f (read-form (new token-reader% [target-string s]))])
    (when (or (eof-object? f) (equal? white-space f))
      (raise-mal-empty))
    f))

;; class for stateful reader object
(define token-reader%
  (class object%
    (init target-string)
    (define current-pos 0)
    (define current-string target-string)
    (super-new)
    (define/private (get-token advance-pos?)
      (cond
        [(>= current-pos (string-length current-string))
         eof]
        [else
;         (printf "matching ~a ~a\n" current-string current-pos)
         (let ([match-list (regexp-match-positions mal-regexp current-string current-pos)])
;           (printf "match-list ~a\n" match-list)
           (cond
;             [(equal? #f match-list)
;              (raise-mal-fail "No match for mal-regexp")]
;             [(equal? 1 (length match-list)) ; No group-match means only white-space
;              (when advance-pos? (set! current-pos (string-length current-string)))
;              white-space]
             [(equal? 2 (length match-list)) ; Whole and group-matches
              (define group-start (caadr match-list))
              (define group-end (cdadr match-list))
              (when advance-pos? (set! current-pos group-end))
              (if (equal? group-start group-end) ; Zero-length match (i.e., nothing except whitespace)
                  white-space
                  (substring current-string group-start group-end))]
             [else (raise-mal-fail "Unexpected in matching mal-regexp")]))]))
    (define/public (next-token)
      (get-token #t))
    (define/public (peek-token)
      (get-token #f))))

;; top-level internal function to read from our reader
(define (read-form r)
  (match (send r peek-token)
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
  (let ([next-form (read-form r)])
    (cond
      [(equal? next-form end-token) '()]
      [(eof-object? next-form) (raise-mal-read "EOF found reading list or vector")]
      [(equal? next-form white-space)  (read-forms-until end-token r)]
      [else (cons next-form (read-forms-until end-token r))])))

;; read an atom
(define (read-atom r)
  (define t (send r next-token))
  (match t
    ["true" #t]
    ["false" #f]
    ["nil" nil]
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
    [(regexp #rx"^;") white-space] ; comment
    [_ (cond
         [(eof-object? t) t]
         [(equal? t white-space) t]
         [else (string->symbol t)])]))

;; regexp for our tokens (at end of file to avoid confusing our editors highlighting)
(define mal-regexp #px"[\\s,]*(~@|[]\\[{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;[^\n]*|[^\\s\\[\\]{}('\"`,;)]*)")

;(equal? (regexp-match-positions mal-regexp "  ") '((0 . 2)))
;(equal? (regexp-match-positions mal-regexp "234 ") '((0 . 3)))
;(regexp-match-positions mal-regexp "234    ")
;(regexp-match-positions mal-regexp ";ab\ncd ")



(module+ test
  (require rackunit rackunit/text-ui)
  (run-tests (test-suite "Reader tests"

   ; empty input gives exceptions
   (check-exn exn:mal:empty? (λ () (read_string "")) "Empty string")
   (check-exn exn:mal:empty? (λ () (read_string "  ")) "White space")

   ; sequences
   (check-equal? (read_string "(1 2 3)") '(1 2 3) "List")
   (check-equal? (read_string "()") '() "Empty list")
   (check-equal? (read_string "[1 2 3]") #(1 2 3) "Vector")
   (check-equal? (read_string "[1,2,4]") #(1 2 4) "Vector with commas")
   (check-equal? (read_string "[]") #() "Empty vector")
   (check-equal? (read_string "(1 (2 3))") '(1 (2 3)) "List of lists")
   (check-equal? (read_string "(()())") '(()()) "Empty list of empty lists")
   (check-equal? (read_string "{ \"a\" 1 \"bb\" 2}") #hash(("a" . 1) ("bb" . 2)) "Hash map")
   (check-equal? (read_string "(()())") '(()()) "Empty hash map")
   (check-exn exn:mal:read? (λ () (read_string "(1 2")) "Unterminated list")
   (check-exn exn:mal:read? (λ () (read_string "(1 2]")) "Wrongly terminated list")
   (check-exn exn:mal:read? (λ () (read_string "[1 2")) "Unterminated vector")
   (check-exn exn:mal:read? (λ () (read_string "{1 2")) "Unterminated hash map")

   ; List with comment inside
   (check-equal? (read_string "(1 2 ;comment\n3)") '(1 2 3) "List with comment")

   ; strings
   (check-equal? (read_string "\"pq\"") "pq" "String")
   (check-equal? (read_string "\"\"") "" "Empty string")
   (check-exn exn:mal:read? (λ () (read_string "\"abc")) "Unterminated string")

   ; numbers
   (check-equal? (read_string "123") 123 "Number")
   (check-equal? (read_string "-45") -45 "Negative number")

   ; special symbols
   (check-equal? (read_string "'pqr") ''pqr "Quote")
   (check-equal? (read_string "`pqr") '`pqr "Quasiquote")
   (check-equal? (read_string "~pqr") ',pqr "Unquote")
   (check-equal? (read_string "^pqr abc") '(with-meta abc pqr) "With-meta")
   (check-equal? (read_string "@pqr") '(deref pqr) "Deref")
   (check-equal? (read_string "~@pqr") '(splice-unquote pqr) "Splice-unquote")

   ; keyword
   (check-equal? (read_string ":pqr") '#:pqr "Keyword")

   ; symbol
   (check-equal? (read_string "pqr") 'pqr "Symbol")
   (check-equal? (read_string "true") #t "Symbol")
   (check-equal? (read_string "false") #f "Symbol")
   (check-equal? (read_string "nil") nil "Symbol")
   )))
