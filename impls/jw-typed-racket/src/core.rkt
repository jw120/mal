#lang typed/racket

(provide core_ns)

(require "printer.rkt" "reader.rkt" "types.rkt" "utils.rkt" "wrap.rkt")

(require/typed readline/readline
               [readline (-> String (U String EOF))])

;; Equality for mal values that equates vectors and lists
(define (mal-equal? [x : Mal] [y : Mal]) : Boolean
  (define (mal-lists-equal? [xs : (Listof Mal)] [ys : (Listof Mal)]) : Boolean
    (and
     (equal? (length xs) (length ys))
     (for/and ([x1 xs] [y1 ys])
       (mal-equal? x1 y1))))
  (define (mal-vectors-equal? [xs : (Vectorof Mal)] [ys : (Vectorof Mal)]) : Boolean
    (and
     (equal? (vector-length xs) (vector-length ys))
     (for/and ([x1 xs] [y1 ys])
       (mal-equal? x1 y1))))  
  (match (cons x y)
    [(cons (mal-list xs) (mal-list ys))
     (mal-lists-equal? xs ys)]
    [(cons (mal-vector xs) (mal-vector ys))
     (mal-vectors-equal? xs ys)]
    [(cons (mal-list xs) (mal-vector ys))
     (mal-vectors-equal? (list->vector xs) ys)]
    [(cons (mal-vector xs) (mal-list ys))
     (mal-vectors-equal? xs (list->vector ys))]
    [(cons (mal-hash x) (mal-hash y))
     (and
      (equal? (hash-count x) (hash-count y))
      (for/and ([k (hash-keys x)])
        (and
         (hash-has-key? y k)
         (mal-equal? (hash-ref x k) (hash-ref y k)))))]
    [_ (equal? x y)]))

(define (mal-concat [xs : (Listof Mal)]) : Mal
  (define (to-list [x : Mal]) : (Listof Mal)
    (match x
      [(mal-list xs) xs]
      [(mal-vector v) (vector->list v)]
      [_ (raise-mal "arguments to concat must be sequences")]))
  (let ([xs-as-lists : (Listof (Listof Mal)) (map to-list xs)])
    (mal-list (apply append xs-as-lists))))

(define core_ns : (Listof (Pair Symbol Mal))
  (list

   ;; Arithmetic and logical functions
   (wrap-binary-int '+ +)
   (wrap-binary-int '- -)
   (wrap-binary-int '* *)
   (wrap-binary-int '/ quotient)
   (wrap-binary-int '< <)
   (wrap-binary-int '> >)
   (wrap-binary-int '<= <=)
   (wrap-binary-int '>= >=)

   ;; Equality and is-a functions
   (wrap-binary '= mal-equal?)
   (wrap-is 'nil? mal-nil?)
   (wrap-is 'true? (λ([x : Mal]) (equal? x #t)))
   (wrap-is 'false? (λ ([x : Mal]) (equal? x #f)))
   (wrap-is 'symbol? symbol?)
   (wrap-is 'atom? box?)
   (wrap-is 'keyword? mal-keyword?)
   (wrap-is 'list? mal-list?)
   (wrap-is 'vector? mal-vector?)
   (wrap-is 'sequential? (λ ([x : Mal]) (or (mal-list? x) (mal-vector? x))))
   (wrap-is 'map? mal-hash?)
   (wrap-is 'fn? mal-function?)
   (wrap-is 'macro? mal-macro?)
   (wrap-is 'string? string?)
   (wrap-is 'number? exact-integer?)
   
   ;; Sequence functions
   (wrap-general 'list mal-list)
   (wrap-unary 'count (match-lambda
                        [(mal-list (list xs ...)) (length xs)]
                        [(mal-vector v) (vector-length v)]
                        [(mal-nil) 0]
                        [_ (raise-mal "Expected a sequence for count")]))
   (wrap-is 'empty? (match-lambda
                      [(mal-list '()) #t]
                      [(mal-vector v) (equal? 0 (vector-length v))]
                      [_ #f]))
   (wrap-binary 'cons (match-lambda**
                       [(x (mal-list ys)) (mal-list (cons x ys))]
                       [(x (mal-vector v)) (mal-list (cons x (vector->list v)))]
                       [(_ _) (raise-mal "bad arguments to cons")]))
   (wrap-general 'concat mal-concat)
   (wrap-unary 'vec (match-lambda
                      [(mal-list xs) (mal-vector (vector->immutable-vector (list->vector xs)))]
                      [(mal-vector v) (mal-vector v)]
                      [_ (raise-mal "expected list or vector as argument to vec")]))
   (wrap-general 'vector (lambda ([params : (Listof Mal)])
                           (mal-vector (vector->immutable-vector (list->vector params)))))
   (wrap-binary 'nth (match-lambda**
                      [((mal-list xs) (? exact-integer? n))
                       (when (or (< n 0) (>= n (length xs)))
                         (raise-mal "nth index out of bounds"))
                       (list-ref xs n)]
                      [((mal-vector v) (? exact-integer? n))
                       (when (or (< n 0) (>= n (vector-length v)))
                         (raise-mal "nth index out of bounds"))
                       (vector-ref v n)]
                      [(_ _) (raise-mal "bad arguments to nth")]))
   (wrap-unary 'first (match-lambda
                        [(mal-list xs)
                         (if (null? xs) (mal-nil) (first xs))]
                        [(mal-vector v)
                         (if (equal? 0 (vector-length v)) (mal-nil) (vector-ref v 0))]
                        [(mal-nil)
                         (mal-nil)]
                        [_
                         (raise-mal "bad arguments to first")]))
   (wrap-unary 'rest (match-lambda
                       [(mal-list '()) (mal-list '())]
                       [(mal-list (cons _ xs)) (mal-list xs)]
                       [(mal-vector (vector)) (mal-list '())]
                       [(mal-vector v) (mal-list (vector->list (vector-drop v 1)))]
                       [(mal-nil) (mal-list '())]
                       [_ (raise-mal "bad arguments to rest")]))
   (wrap-unary 'seq (match-lambda
                      [(mal-list '()) (mal-nil)]
                      [(mal-vector (vector)) (mal-nil)]
                      ["" (mal-nil)]
                      [(mal-nil) (mal-nil)]
                      [(mal-list xs) (mal-list xs)]
                      [(mal-vector v) (mal-list (vector->list v))]
                      [(? string? s) (mal-list (cdr (drop-right (string-split s "") 1)))]
                      [_ (raise-mal "bad argument to seq")]))
   (wrap-general 'conj (match-lambda
                        [(list (mal-list xs) elements ...)
                        (mal-list (append (reverse elements) xs))]
                        [(list (mal-vector v) elements ...)
                         (mal-vector
                          (vector->immutable-vector (vector-append v (list->vector elements))))]
                        [_ (raise-mal "bad arguments to conj")]))

   ;; Hashmap-related function
   (wrap-general 'hash-map (λ ([vals : (Listof Mal)]) (mal-hash (flat-list->mal-hashmap vals))))
   (wrap-unary 'keys (match-lambda
                       [(mal-hash m) (mal-list (hash-keys m))]
                       [_ (raise-mal "argument for keys must be a hashmap")]))
   (wrap-unary 'vals (match-lambda
                       [(mal-hash m) (mal-list (hash-values m))]
                       [_ (raise-mal "argument for keys must be a hashmap")]))
   (wrap-binary 'contains? (match-lambda**
                            [((mal-hash m) k) (hash-has-key? m k)]
                            [(_ _) (raise-mal "contains takes a hashmap and a key value")]))
   (wrap-binary 'get (match-lambda**
                      [((mal-hash m) (? mal-hashkey? k)) (hash-ref m k (λ () (mal-nil)))]
                      [((mal-nil) _) (mal-nil)]
                      [(_ _) (raise-mal "get takes a hashmap and a key value")]))
   (wrap-general 'assoc (match-lambda
                          [(list (mal-hash m) args ...)
                           (define new-pairs : (Immutable-HashTable MalHashKey Mal)
                             (flat-list->mal-hashmap args))
                           (mal-hash
                            (for/fold ([updated-m : (Immutable-HashTable MalHashKey Mal) m])
                                      ([(k v) new-pairs])
                              (hash-set updated-m k v)))]
                          [_ (raise-mal "first argument of assoc must be a hashmap")]))
   (wrap-general 'dissoc (match-lambda
                           [(list (mal-hash m) keys ...)
                            (mal-hash
                             (for/fold ([updated-m : (Immutable-HashTable MalHashKey Mal) m])
                                       ([k keys])
                               (hash-remove updated-m k)))]
                           [_ (raise-mal "bad argments for dissoc")]))
   
   ;; Atom-related functions
   (wrap-unary 'atom box)
   (wrap-unary 'deref (match-lambda 
                        [(box b) b]
                        [_ (raise-mal "need an atom to deref")]))
   (wrap-binary 'reset!  (match-lambda**
                          [((? box? b) val)
                           (set-box! b val)
                           val]
                          [(_ _) (raise-mal "need an atom and a value for reset!")]))
   (wrap-general 'swap! (match-lambda 
                          [(list (? box? b) (mal-function f) other-args ...)
                           (let* ([args : (Listof Mal) (cons (unbox b) other-args)]
                                  [new-val : Mal (f args)])
                             (set-box! b new-val)
                             new-val)]
                          [_ (raise-mal "need an atom, a function for swap!")]))

   ;; IO functions
   (wrap-unary-str 'read-string read_str)
   (wrap-unary-str 'slurp file->string)
   (wrap-general 'prn (lambda ([args : (Listof Mal)])
                        (let ([s : String (string-join (map (lambda ([x : Mal]) (pr_str x #t)) args) " ")])
                          (displayln s)
                          (mal-nil))))
   (wrap-general 'println (lambda ([args : (Listof Mal)])
                            (let ([s : String (string-join (map (lambda ([x : Mal]) (pr_str x #f)) args) " ")])
                              (displayln s)
                              (mal-nil))))
   (wrap-general 'pr-str (lambda ([args : (Listof Mal)])
                           (string-join (map (lambda ([x : Mal]) (pr_str x #t)) args) " ")))
   (wrap-general 'str (lambda ([args : (Listof Mal)])
                        (string-join (map (lambda ([x : Mal]) (pr_str x #f)) args) "")))
   (wrap-unary-str 'readline (lambda (prompt)
                               (let ([s (readline prompt)])
                                 (if (eof-object? s) (mal-nil) s))))

   
   ;; Misc functions
   (cons '*host-language* "jw-typed-racket")
   (wrap-general 'time-ms (match-lambda
                            ['()
                             (exact-round (current-inexact-milliseconds))]
                            [_ (raise-mal "expect no arguments for time-ms")]))
   (wrap-unary-str 'symbol string->symbol)
   (wrap-unary 'keyword (match-lambda
                          [(? string? s) (mal-keyword s)]
                          [(mal-keyword k) (mal-keyword k)]
                          [_ (raise-mal "bad argument to keyword")]))
   (wrap-unary 'throw raise-mal)
   (wrap-binary 'map (match-lambda**
                      [((mal-function f) (mal-list xs))
                       (define (f1 [x : Mal]) ; convert to Mal->Mal function
                         (f (list x)))
                       (mal-list (map f1 xs))]
                      [((mal-function f) (mal-vector v))
                       (define (f1 [x : Mal])
                         (f (list x)))
                       (mal-list (map f1 (vector->list v)))]
                      [(_ _)
                       (raise-mal "bad arguments to map")]))
   (wrap-general 'apply (lambda ([args : (Listof Mal)])
                          (when (< (length args) 2)
                            (raise-mal "need two args for apply"))
                          (define apply-fn : (-> (Listof Mal) Mal)
                            (match (car args)
                              [(mal-function f) f]
                              [_ (raise-mal "bad first arg for apply")]))                                       
                          (define apply-args : (Listof Mal)
                            (append (drop-right (cdr args) 1) ; args without first and last
                                    (match (last args)
                                      [(mal-list xs) xs]
                                      [(mal-vector v) (vector->list v)]
                                      [_ (raise-mal "bad last arg for apply")])))
                          (apply-fn apply-args)))
   (wrap-general 'meta (lambda (args) "NYI"))
   (wrap-general 'with-meta (lambda (args) "NYI"))
   
   ))



#|
  

   (cons 'conj (lambda args
                 (when (< (length args) 2)
                   (raise-mal-eval "Bad arguments to conj"))
                 (define collection (car args))
                 (define adds (cdr args))
                 (cond
                   [(list? collection) (append (reverse adds) collection)]
                   [(vector? collection) (vector-append collection (list->vector adds))]
                   [else (raise-mal-eval "Bad collection type for conj")])))

 

   (cons 'meta (λ (x) (cond
                        [(func? x) (func-meta x)]
                        [(procedure? x) nil]
                        [else (raise-mal-eval "meta not implemented for non-functions")])))
   (cons 'with-meta (λ (x y) (cond
                               [(func? x) (func (func-is-macro? x) (func-closure x) y)]
                               [(procedure? x) (func #f x y)]
                               [else (raise-mal-eval "with-meta not implemented for non-functions")])))
   (cons 'time-ms (λ args (round (current-inexact-milliseconds))))

   ))
|#