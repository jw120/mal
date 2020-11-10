#lang typed/racket

(require "types.rkt" "utils.rkt")

(provide read_string)

;; top-level reading function which we export. Sets up reader and hands over to read-form
;; If no input (or just spaces/comments) raise the empty exception
(define (read_string [s : String]) : Mal
  (define result : (U Mal EOF)
    (read-possible-form (make-token-reader s)))
  (display "result") (displayln result)
  (if (or (eof-object? result)
          (and (string? result) (string-whitespace? result)))
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
            [(list (cons whole-start whole-end) (cons group-start group-end))
             (when advance (set! pos group-end))
             (if (equal? group-start group-end) ; Zero-length match means nothing but white-space/comments
                 " "
                 (substring str group-start group-end))]
            [_ (raise-mal-failure "unexpected match result in token-reader")])))
    (lambda (command)
      (cond [(equal? command 'next!) (get-token true)]
            [(equal? command 'peek) (get-token false)]
            [else (raise-mal-failure "bad command to token-reader")]))))

;; top-level internal function to read from our reader
(define (read-possible-form [r : token-reader]) : (U Mal EOF)
  (match (r 'peek)
    ["("
     (r 'next!) ; skip the "("
     (mal-list (read-forms-until ")" r))]
    ["["
     (r 'next!) ; skip the "["
     (mal-vector
      (vector->immutable-vector
       (list->vector
        (read-forms-until "]" r))))]
    ["{"
     (r 'next!) ; skip the "{"
     (mal-hash (flat-list->mal-hashmap (read-forms-until "}" r)))]
    [s (cond
         [(eof-object? s) eof]
         [(string-whitespace? s) s]
         [else (read-atom r)])]))

(define (read-form [r : token-reader]) : Mal
  (let ([possible-form (read-possible-form r)])
    (cond
      [(eof-object? possible-form)
       (raise-mal "Unexpected EOF reading a form")]
      [(and (string? possible-form) (string-whitespace? possible-form))
       (raise-mal "Found only whitespace or comment when reading a form")]
      [else
       possible-form])))

;; read a list of forms until (but not including) the given token
(define (read-forms-until [end-token : String] [r : token-reader]) : (Listof Mal)
  (let ([next-form (read-possible-form r)])
    (cond
      [(equal? next-form end-token) '()]
      [(and (string? next-form) (string-whitespace? next-form)) (read-forms-until end-token r)]
      [(eof-object? next-form) (raise-mal "EOF found reading list or vector")]
      [else (cons next-form (read-forms-until end-token r))])))

;; read an atom
(define (read-atom [r : token-reader]) : Mal
  (define token : (U String EOF) (r 'next!))
  (if (eof-object? token)
      (raise-mal "Unexpected EOF in read-atom")
      (match token
        ["true" #t]
        ["false" #f]
        ["nil" (mal-nil)]
        ["'" (mal-list (list 'quote (read-form r)))]
        ["`" (mal-list (list 'quasiquote (read-form r)))]
        ["~" (mal-list (list 'unquote (read-form r)))]
        ["^"
         (let* ([x (read-form r)]
                [y (read-form r)])
           (mal-list (list 'with-meta y x)))]
        ["@" (mal-list (list 'deref (read-form r)))]
        ["~@" (mal-list (list 'splice-unquote (read-form r)))]
        [(regexp #px"^[{}()\\[\\]]") token] ; tokens for delimiters (returned as strings)
        [(regexp #px"^-?[[:digit:]]+$") (string->int token)] ; number
        [(regexp #rx"^\\\".*\\\"$") (remove-escapes (substring token 1 (- (string-length token) 1)))] ; quoted string
        [(regexp #rx"^\\\".*$") (raise-mal "EOF reading string")]
        ;[(regexp #rx"^:.+$")  (string->keyword (substring token 1))] ; keyword
        [(regexp #rx"^;") " "] ; comment
        [_ (cond
             [(string-whitespace? token) token]
             [else (string->symbol token)])])))




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
  (require typed/rackunit)

  ; empty input gives exceptions
  (check-exn exn:mal-empty? (λ () (read_string "")) "Empty string")
  (check-exn exn:mal-empty? (λ () (read_string "  ")) "White space")
  (check-exn exn:mal-empty? (λ () (read_string ";qqq")) "Comment only")

    
  ; sequences
  
  (check-equal? (read_string "(1 2 3)")
                (mal-list '(1 2 3)) "List")
  (check-equal? (read_string "()")
                (mal-list '()) "Empty list")
  (check-equal? (read_string "[1 2 3]")
                (mal-vector #(1 2 3)) "Vector")
  (check-equal? (read_string "[1,2,4]")
                (mal-vector #(1 2 4)) "Vector with commas")
  (check-equal? (read_string "[]")
                (mal-vector #()) "Empty vector")  
  (check-equal? (read_string "(1 (2 3))")
                (mal-list (list 1 (mal-list (list 2 3)))) "List of lists")
  (check-equal? (read_string "(()())")
                (mal-list (list (mal-list '()) (mal-list '()))) "Empty list of empty lists")
  (check-equal? (read_string "{ \"a\" 1 \"bb\" 2}")
                (mal-hash #hash(("a" . 1) ("bb" . 2))) "Hash map")
  (check-exn exn:mal? (λ () (read_string "(1 2")) "Unterminated list")
  (check-exn exn:mal? (λ () (read_string "(1 2]")) "Wrongly terminated list")
  (check-exn exn:mal? (λ () (read_string "[1 2")) "Unterminated vector")
  (check-exn exn:mal? (λ () (read_string "{1 2")) "Unterminated hash map")

  ; List with comment inside
  (check-equal? (read_string "(1 2 ;comment\n3)")
                (mal-list '(1 2 3)) "List with comment")

  ; strings
  (check-equal? (read_string "\"pq\"") "pq" "String")
  (check-equal? (read_string "\"\"") "" "Empty string")
  (check-exn exn:mal? (λ () (read_string "\"abc")) "Unterminated string")

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
  (check-equal? (read_string "true") #t "True")
  (check-equal? (read_string "false") #f "False")
  (check-equal? (read_string "nil") (mal-nil) "Nil")

  ; Correctly handle comment then atom
  (define test-r (make-token-reader ";comment\n21"))
  (check-equal? (read-form test-r) " " "Comment then number - comment")
  (check-equal? (read-form test-r) 21 "Comment then number - number")
  )
