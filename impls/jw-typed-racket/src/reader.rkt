#lang typed/racket

(require "types.rkt" "utils.rkt")

(provide read_string)

;; top-level reading function which we export. Sets up reader and hands over to read-form
;; If no input (or just spaces/comments) raise the empty exception
(define (read_string [s : String]) : Mal
  (define result : (U Mal EOF)
    (read-possible-form (make-token-reader s)))
  (if (eof-object? result)
      (raise-mal-empty)
      result))
  
;; Token reader holds mutable state with the current position in the string 
(define-type token-reader (-> (U 'next! 'peek) (U String EOF)))
(define (make-token-reader [s : String]) : token-reader
  (let ([reader-pos : Index 0] ; reader-pos is mutated below
        [str : String s])
    (define (get-token [get-pos : Index]) : (Values (U String EOF) Index)
      (if (>= get-pos (string-length str))
          (values eof get-pos)
          (match (regexp-match-positions mal-regexp str get-pos)
            [(list (cons whole-start whole-end) (cons group-start group-end))
             (if (or
                  (equal? group-start group-end) ; Zero-length match means nothing but white-space
                  (regexp-match mal-comment-regexp str group-start group-end))
                 (get-token group-end) ; Look again if a space/comment
                 (values (substring str group-start group-end) group-end))]
            [_ (raise-mal-failure "unexpected match result in token-reader")])))
    (lambda (command)
      (if (or (equal? command 'next!) (equal? command 'peek))
          (let-values ([(result new-pos) (get-token reader-pos)])
            (when (equal? command 'next!)
              (set! reader-pos new-pos))
            result)
          (raise-mal-failure "bad command to token reader")))))

;; top-level reading function (this returns EOF if no more tokens, other read- functions fail on EOF)
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
    [s (if (eof-object? s)
           eof
           (read-atom r))]))

(define (read-form [r : token-reader]) : Mal
  (let ([possible-form (read-possible-form r)])
    (if (eof-object? possible-form)
        (raise-mal "Unexpected EOF reading a form")
        possible-form)))

;; read a list of forms until (but not including) the given token
(define (read-forms-until [end-token : String] [r : token-reader]) : (Listof Mal)
  (let ([next-form (read-possible-form r)])
    (cond
      [(equal? next-form end-token) '()]
      [(eof-object? next-form) (raise-mal "EOF found reading list or vector")]
      [else (cons next-form (read-forms-until end-token r))])))

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
        [(regexp #rx"^\\\".*$") (raise-mal "EOF reading string")] ; Mismatched double-quote
        [(regexp #rx"^:.+$")  (mal-keyword (substring token 1))] ; keyword
        [sym (string->symbol token)])))

;; regexp for our tokens
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

;; regexp for coment tokens (that our token reader skips over)
(define mal-comment-regexp : Regexp
  (pregexp "^;[^\n]*$"))


(module+ test
  (require typed/rackunit)

  ; empty input gives exceptions
  (check-exn exn:mal-empty? (λ () (read_string "")) "Empty string")
  (check-exn exn:mal-empty? (λ () (read_string "  ")) "White space")
  (check-exn exn:mal-empty? (λ () (read_string ";qqq")) "Comment only")

  ; token reader skips spaces and comments
  (check-equal? ((make-token-reader "  ") 'next!) eof)
  (check-equal? ((make-token-reader "  ; ") 'next!) eof)
  (check-equal? ((make-token-reader "  ;abc \n2") 'next!) "2")

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
  (check-equal? (read_string "'pqr") (mal-list '(quote pqr)) "Quote")
  (check-equal? (read_string "`pqr") (mal-list '(quasiquote pqr)) "Quasiquote")
  (check-equal? (read_string "~pqr") (mal-list '(unquote pqr)) "Unquote")
  (check-equal? (read_string "^pqr abc") (mal-list '(with-meta abc pqr)) "With-meta")
  (check-equal? (read_string "@pqr") (mal-list '(deref pqr)) "Deref")
  (check-equal? (read_string "~@pqr") (mal-list '(splice-unquote pqr)) "Splice-unquote")

  ; keyword
  (check-equal? (read_string ":pqr") (mal-keyword "pqr") "Keyword")

  ; symbol
  (check-equal? (read_string "pqr") 'pqr "Symbol")
  (check-equal? (read_string "true") #t "True")
  (check-equal? (read_string "false") #f "False")
  (check-equal? (read_string "nil") (mal-nil) "Nil"))
