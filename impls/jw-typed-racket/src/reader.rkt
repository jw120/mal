#lang typed/racket

(require "types.rkt")

(provide read_string)

; We use the symbols eof (defined by system) and white-space (defined here)
; to match inside this file (end of input and spaces/comments respectively)
; They are converted to the empty exception in read_string
;(define white-space (string->uninterned-symbol "white-space"))

;; top-level reading function which we export. Sets up reader and hands over to read-form
;; If no input (or just space/comment raise the empty exception
(define (read_string [s : String]) : Mal
  (define result : (U Mal EOF)
    (read-form (make-token-reader s)))
  (if (or (eof-object? result) (string-whitespace? result))
      (raise-mal-empty)
      result))
  
;; Token reader holds mutable state with the current position in the string 
(define-type token-reader (-> (U 'next! 'peek) (U String EOF)))
(define (make-token-reader [s : String]) : token-reader
  (let ([pos : Nonnegative-Integer 0] ; pos is mutated below
        [str : String s])
    (define (get-token [advance : Boolean]) : (U String EOF)
      (if (>= pos (string-length str))
          eof
          (match (regexp-match-positions mal-regexp str pos)
            [(list (cons group-start group-end))
             (when advance (set! pos group-end))
             (if (equal? group-start group-end) ; Zero-length match means nothing but white-space
                 " "
                 (substring str group-start group-end))]
            [_ (raise-mal-failure "unexpected match result in token-reader")])))
    (lambda (command)
      (cond [(equal? command 'next!) (get-token true)]
            [(equal? command 'peek) (get-token false)]
            [else (raise-mal-failure "bad command to token-reader")]))))

;; top-level internal function to read from our reader
(define (read-form [r : token-reader]) : (U Mal EOF)
  (match (r 'peek)
    ["(" (mal-list (mal-nil) (read-list r))]
    ["[" (mal-vector (mal-nil) (read-vector r))]
    ["{" (mal-hash (mal-nil) (read-hash-map r))]
    EOF
    [_ (read-atom r)]))

(define (read-list [r : token-reader]) : (Listof Mal)
  (unless (equal? (r 'next!) "(")
    (raise-mal-fail "no opening paren in read-list"))
  (read-forms-until ")" r))

(define (read-vector [r : token-reader]) : (Immutable-Vectorof Mal)
  (unless (equal? (r 'next!) "[")
    (raise-mal-fail "no opening bracker in read-vector"))
  (vector->immutable-vector
   (list->vector
    (read-forms-until "]" r))))

(define (read-hash-map [r : token-reader]) : (Immutable-HashTable String Mal)
  (unless (equal? (r 'next!) "{")
    (raise-mal-fail "no opening brace in read-hash-map"))
  (apply hash (read-forms-until "}" r)))

;; read a list of forms until (but not including) the given token
(define (read-forms-until [end-token : String] [r : token-reader]) : (Listof String)
  (let ([next-form (read-form r)])
    (cond
      [(equal? next-form end-token) '()]
      [(eof-object? next-form) (raise-mal-read "EOF found reading list or vector")]
      [(equal? next-form 'whitespace)  (read-forms-until end-token r)]
      [else (cons next-form (read-forms-until end-token r))])))

;; read an atom
(define (read-atom [r : token-reader]) : Mal
  (define t : String (r 'next!))
  (match t
    ["true" #t]
    ["false" #f]
    ["nil" (mal-nil)]
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
    [(regexp #rx"^;") 'whitespace] ; comment
    [_ (cond
         [(eof-object? t) t]
         [(equal? t 'whitespace) t]
         [else (string->symbol t)])]))

(define (remove-escapes [s : String]) : String
  (define (accumulate [acc : String] [in-escape : Boolean] [ch : Char]) : (Values String Boolean)
     (match (list in-escape ch)
            [(list #t #\n) (values (string-append acc "\n") #f)]
            [(list #t #\") (values (string-append acc "\"") #f)]
            [(list #t #\\) (values (string-append acc "\\") #f)]
            [(list #t x) (raise-mal-read (format "Unknown escape ~a in remove-escapes" ch))]
            [(list #f #\\) (values acc #t)]
            [(list #f x) (values (string-append acc (string ch)) #f)]))    
  (let-values
      ([(result still-in-escape)
        (for/fold ([acc : String ""]
                   [in-escape : Boolean #f])
                  ([ch : Char (in-string s)])
          (accumulate acc in-escape ch))])
    (if still-in-escape
        (raise-mal-read "Escape unbalanced in remove-escapes")
        result)))

(define (string-whitespace? [s: String]) : Boolean
  (for/and ([ch (in-string s)])
    (char-whitespace? ch)))
(module+ test
  (require rackunit)
  (check-true (string-whitespace "   "))
  (check-true (string-whitespace ""))
  (check-false (string-whitespace " a")))


;; regexp for our tokens (at end of file to avoid confusing our editors highlighting)
(define mal-regexp : Regexp
  (pregexp
   (string-append
    "[\\s,]*" ; leading white-space (including commas) skipped (as outside of regexp group)
    "("       ; regexp group used to match tokens
    (string-join
     '("~@"                        ; two-character token
       "[]\\[{}()'`~^@]"           ; one-character tokens
       "\"(?:\\\\.|[^\\\\\"])*\"?" ; quoted-string
       ";[^\n]*"                   ; comment
       "[^\\s\\[\\]{}('\"`,;)]*")  ; string of non-special characters
     "|")
    ")")))

(module+ test
  (require rackunit)

  ; empty input gives exceptions
  (check-exn exn:mal:empty? (λ () (read_string "")) "Empty string")
  (check-exn exn:mal:empty? (λ () (read_string "  ")) "White space")
  (check-exn exn:mal:empty? (λ () (read_string ";qqq")) "Comment only")

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

  ; Correctly handle comment then atom
  (define r (new token-reader% [target-string ";comment\n21"]))
  (check-equal? (read-form r) white-space "Comment then number - comment")
  (check-equal? (read-form r) 21 "Comment then number - number")
  )
