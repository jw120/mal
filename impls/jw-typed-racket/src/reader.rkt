#lang typed/racket

(require "types.rkt" "utils.rkt")

(provide read_str)

;; top-level reading function which we export. Sets up reader and hands over to read-form
;; If no input (or just spaces/comments) raise the empty exception
(define (read_str [s : String]) : Mal
  (define result : (U Mal EOF)
    (read-possible-form (make-token-reader s)))
  (if (eof-object? result)
      (void)
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
     (mal-list #f #f (read-forms-until ")" r))]
    ["["
     (r 'next!) ; skip the "["
     (mal-vector #f #f 
      (vector->immutable-vector
       (list->vector
        (read-forms-until "]" r))))]
    ["{"
     (r 'next!) ; skip the "{"
     (mal-hash #f #f (flat-list->mal-hashmap (read-forms-until "}" r)))]
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
        ["'" (mal-list #f #f (list 'quote (read-form r)))]
        ["`" (mal-list #f #f (list 'quasiquote (read-form r)))]
        ["~" (mal-list #f #f (list 'unquote (read-form r)))]
        ["^"
         (let* ([x (read-form r)]
                [y (read-form r)])
           (mal-list #f #f (list 'with-meta y x)))]
        ["@" (mal-list #f #f (list 'deref (read-form r)))]
        ["~@" (mal-list #f #f (list 'splice-unquote (read-form r)))]
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

  ; empty input gives void
  (check-true (void? (read_str "")) "Empty string")
  (check-true (void? (read_str "  ")) "White space")
  (check-true (void? (read_str ";qqq")) "Comment only")

  ; token reader skips spaces and comments
  (check-equal? ((make-token-reader "  ") 'next!) eof)
  (check-equal? ((make-token-reader "  ; ") 'next!) eof)
  (check-equal? ((make-token-reader "  ;abc \n2") 'next!) "2")

  ; sequences  
  (check-equal? (read_str "(1 2 3)")
                (mal-list #f #f'(1 2 3)) "List")
  (check-equal? (read_str "()")
                (mal-list #f #f '()) "Empty list")
  (check-equal? (read_str "[1 2 3]")
                (mal-vector #f #f #(1 2 3)) "Vector")
  (check-equal? (read_str "[1,2,4]")
                (mal-vector #f #f #(1 2 4)) "Vector with commas")
  (check-equal? (read_str "[]")
                (mal-vector #f #f #()) "Empty vector")  
  (check-equal? (read_str "(1 (2 3))")
                (mal-list #f #f (list 1 (mal-list #f #f (list 2 3)))) "List of lists")
  (check-equal? (read_str "(()())")
                (mal-list #f #f (list (mal-list #f #f '()) (mal-list #f #f '()))) "Empty list of empty lists")
  (check-equal? (read_str "{ \"a\" 1 \"bb\" 2}")
                (mal-hash #f #f #hash(("a" . 1) ("bb" . 2))) "Hash map")
  (check-exn exn:mal? (λ () (read_str "(1 2")) "Unterminated list")
  (check-exn exn:mal? (λ () (read_str "(1 2]")) "Wrongly terminated list")
  (check-exn exn:mal? (λ () (read_str "[1 2")) "Unterminated vector")
  (check-exn exn:mal? (λ () (read_str "{1 2")) "Unterminated hash map")

  ; List with comment inside
  (check-equal? (read_str "(1 2 ;comment\n3)")
                (mal-list #f #f '(1 2 3)) "List with comment")

  ; strings
  (check-equal? (read_str "\"pq\"") "pq" "String")
  (check-equal? (read_str "\"\"") "" "Empty string")
  (check-exn exn:mal? (λ () (read_str "\"abc")) "Unterminated string")

  ; numbers
  (check-equal? (read_str "123") 123 "Number")
  (check-equal? (read_str "-45") -45 "Negative number")

  ; special symbols
  (check-equal? (read_str "'pqr") (mal-list #f #f '(quote pqr)) "Quote")
  (check-equal? (read_str "`pqr") (mal-list #f #f '(quasiquote pqr)) "Quasiquote")
  (check-equal? (read_str "~pqr") (mal-list #f #f '(unquote pqr)) "Unquote")
  (check-equal? (read_str "^pqr abc") (mal-list #f #f '(with-meta abc pqr)) "With-meta")
  (check-equal? (read_str "@pqr") (mal-list #f #f '(deref pqr)) "Deref")
  (check-equal? (read_str "~@pqr") (mal-list #f #f '(splice-unquote pqr)) "Splice-unquote")

  ; keyword
  (check-equal? (read_str ":pqr") (mal-keyword "pqr") "Keyword")

  ; symbol
  (check-equal? (read_str "pqr") 'pqr "Symbol")
  (check-equal? (read_str "true") #t "True")
  (check-equal? (read_str "false") #f "False")
  (check-equal? (read_str "nil") (mal-nil) "Nil"))
