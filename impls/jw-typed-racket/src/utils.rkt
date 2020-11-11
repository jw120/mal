#lang typed/racket

(require "types.rkt")

(provide (all-defined-out))


(define (add-escapes [s : String]) : String
  (define translated-chars : (Listof String)
    (for/list ([ch (in-string s)])
      (match ch
        [#\newline "\\n"]
        [#\" "\\\""]
        [#\\ "\\\\"]
        [c (string c)])))
  (string-append* translated-chars))
(module+ test
  (require typed/rackunit)
  (check-equal? (add-escapes "a\"b") "a\\\"b" "Escapes double-quote")
  (check-equal? (add-escapes "a\nb") "a\\nb" "Escapes newline")
  (check-equal? (add-escapes "a\\b") "a\\\\b" "Escapes backslash"))


(define (remove-escapes [s : String]) : String
  (define (accumulate [acc : String] [in-escape : Boolean] [ch : Char]) : (Values String Boolean)
    (match (list in-escape ch)
      [(list #t #\n) (values (string-append acc "\n") #f)]
      [(list #t #\") (values (string-append acc "\"") #f)]
      [(list #t #\\) (values (string-append acc "\\") #f)]
      [(list #t x) (raise-mal (format "Unknown escape ~a in remove-escapes" ch))]
      [(list #f #\\) (values acc #t)]
      [(list #f x) (values (string-append acc (string ch)) #f)]))    
  (let-values
      ([(result still-in-escape)
        (for/fold ([acc : String ""]
                   [in-escape : Boolean #f])
                  ([ch : Char (in-string s)])
          (accumulate acc in-escape ch))])
    (if still-in-escape
        (raise-mal "Escape unbalanced in remove-escapes")
        result)))
(module+ test
  (require typed/rackunit)
  (check-equal? (remove-escapes "a\\\"b") "a\"b" "Escapes double-quote")
  (check-equal? (remove-escapes "a\\nb") "a\nb" "Escapes newline")
  (check-equal? (remove-escapes "a\\\\b") "a\\b" "Escapes backslash")
  (check-exn exn:mal? (λ () (remove-escapes "a\\")) "Incomplete escape")
  (check-exn exn:mal? (λ () (remove-escapes "a\\Q")) "Unknown escape"))
  


(define (string-whitespace? [s : String]) : Boolean
  (for/and ([ch (in-string s)])
    (char-whitespace? ch)))
(module+ test
  (require typed/rackunit)
  (check-true (string-whitespace? "   "))
  (check-true (string-whitespace? ""))
  (check-false (string-whitespace? " a")))


(define (mal-hashmap->flat-list [m : (Immutable-HashTable MalHashKey Mal)]) : (Listof Mal)
  (define kv-list : (Listof (Pairof MalHashKey Mal)) (hash->list m))
  (define (go [xs : (Listof (Pairof Mal Mal))]) : (Listof Mal)
    (if (null? xs)
        '()
        (cons (caar xs) (cons (cdar xs) (go (cdr xs))))))
  (go kv-list))
(module+ test
  (require typed/rackunit)
  (check-equal?
   (mal-hashmap->flat-list #hash(("a" . 2)))
   (list "a" 2)))


(define (flat-list->mal-hashmap [val-list : (Listof Mal)]) : (Immutable-HashTable MalHashKey Mal)
  (define (go [m : (Immutable-HashTable MalHashKey Mal)] [xs : (Listof Mal)]) : (Immutable-HashTable MalHashKey Mal)
    (cond [(null? xs)
           m]
          [(null? (cdr xs))
           (raise-mal "Odd number of list elements making hash")]
          [(or (string? (car xs)) (mal-keyword? (car xs)))
           (go (hash-set m (car xs) (cadr xs)) (cddr xs))]
          [else
           (raise-mal "Keys must be strings making hash")]))
  (go (make-immutable-hash) val-list))
(module+ test
  (require typed/rackunit)
  (check-equal? (flat-list->mal-hashmap (list "a" 2)) #hash(("a" . 2)))
  (check-exn exn:mal? (λ () (flat-list->mal-hashmap (list 2 2))))
  (check-exn exn:mal? (λ () (flat-list->mal-hashmap (list "a" 2 "b")))))


(define (string->int [s : String]) : Integer
  (let ([x (string->number s)])
    (if (exact-integer? x)
        x
        (raise-mal "Expected an integer"))))
(module+ test
  (require typed/rackunit)
  (check-equal? (string->int "12") 12)
  (check-exn exn:mal? (λ () (string->int "123.45")))
  (check-exn exn:mal? (λ () (string->int "QQ"))))