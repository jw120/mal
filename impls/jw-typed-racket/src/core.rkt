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
    [(cons (mal-list _ _ xs) (mal-list _ _ ys))
     (mal-lists-equal? xs ys)]
    [(cons (mal-vector _ _ xs) (mal-vector _ _ ys))
     (mal-vectors-equal? xs ys)]
    [(cons (mal-list _ _ xs) (mal-vector _ _ ys))
     (mal-vectors-equal? (list->vector xs) ys)]
    [(cons (mal-vector _ _ xs) (mal-list _ _ ys))
     (mal-vectors-equal? xs (list->vector ys))]
    [(cons (mal-hash _ _ x) (mal-hash _ _ y))
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
      [(mal-list _ _ xs) xs]
      [(mal-vector _ _ v) (vector->list v)]
      [_ (raise-mal "arguments to concat must be sequences")]))
  (let ([xs-as-lists : (Listof (Listof Mal)) (map to-list xs)])
    (mal-list #f #f (apply append xs-as-lists))))

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
   (wrap-general 'list (λ (args) (mal-list #f #f args)))
   (wrap-unary 'count (match-lambda
                        [(mal-list _ _ (list xs ...)) (length xs)]
                        [(mal-vector _ _ v) (vector-length v)]
                        [(mal-nil) 0]
                        [_ (raise-mal "Expected a sequence for count")]))
   (wrap-is 'empty? (match-lambda
                      [(mal-list _ _ '()) #t]
                      [(mal-vector _ _ v) (equal? 0 (vector-length v))]
                      [_ #f]))
   (wrap-binary 'cons (match-lambda**
                       [(x (mal-list _ _ ys)) (mal-list #f #f (cons x ys))]
                       [(x (mal-vector _ _ v)) (mal-list #f #f (cons x (vector->list v)))]
                       [(_ _) (raise-mal "bad arguments to cons")]))
   (wrap-general 'concat mal-concat)
   (wrap-unary 'vec (match-lambda
                      [(mal-list _ _ xs) (mal-vector #f #f (vector->immutable-vector (list->vector xs)))]
                      [(mal-vector _ _ v) (mal-vector #f #f v)]
                      [_ (raise-mal "expected list or vector as argument to vec")]))
   (wrap-general 'vector (lambda ([params : (Listof Mal)])
                           (mal-vector #f #f (vector->immutable-vector (list->vector params)))))
   (wrap-binary 'nth (match-lambda**
                      [((mal-list _ _ xs) (? exact-integer? n))
                       (when (or (< n 0) (>= n (length xs)))
                         (raise-mal "nth index out of bounds"))
                       (list-ref xs n)]
                      [((mal-vector _ _ v) (? exact-integer? n))
                       (when (or (< n 0) (>= n (vector-length v)))
                         (raise-mal "nth index out of bounds"))
                       (vector-ref v n)]
                      [(_ _) (raise-mal "bad arguments to nth")]))
   (wrap-unary 'first (match-lambda
                        [(mal-list _ _ xs)
                         (if (null? xs) (mal-nil) (first xs))]
                        [(mal-vector _ _ v)
                         (if (zero? (vector-length v)) (mal-nil) (vector-ref v 0))]
                        [(mal-nil)
                         (mal-nil)]
                        [_
                         (raise-mal "bad arguments to first")]))
   (wrap-unary 'rest (match-lambda
                       [(mal-list _ _ '()) (mal-list #f #f '())]
                       [(mal-list _ _(cons _ xs)) (mal-list #f #f xs)]
                       [(mal-vector _ _(vector)) (mal-list #f #f '())]
                       [(mal-vector _ _ v) (mal-list #f #f (vector->list (vector-drop v 1)))]
                       [(mal-nil) (mal-list #f #f '())]
                       [_ (raise-mal "bad arguments to rest")]))
   (wrap-unary 'seq (match-lambda
                      [(mal-list _ _ '()) (mal-nil)]
                      [(mal-vector _ _ (vector)) (mal-nil)]
                      ["" (mal-nil)]
                      [(mal-nil) (mal-nil)]
                      [(mal-list _ _ xs) (mal-list #f #f xs)]
                      [(mal-vector _ _ v) (mal-list #f #f (vector->list v))]
                      [(? string? s) (mal-list #f #f (cdr (drop-right (string-split s "") 1)))]
                      [_ (raise-mal "bad argument to seq")]))
   (wrap-general 'conj (match-lambda
                        [(list (mal-list _ _ xs) elements ...)
                        (mal-list #f #f (append (reverse elements) xs))]
                        [(list (mal-vector _ _ v) elements ...)
                         (mal-vector #f #f
                          (vector->immutable-vector (vector-append v (list->vector elements))))]
                        [_ (raise-mal "bad arguments to conj")]))

   ;; Hashmap-related function
   (wrap-general 'hash-map (λ ([vals : (Listof Mal)]) (mal-hash #f #f (flat-list->mal-hashmap vals))))
   (wrap-unary 'keys (match-lambda
                       [(mal-hash _ _ m) (mal-list #f #f (hash-keys m))]
                       [_ (raise-mal "argument for keys must be a hashmap")]))
   (wrap-unary 'vals (match-lambda
                       [(mal-hash _ _ m) (mal-list #f #f (hash-values m))]
                       [_ (raise-mal "argument for keys must be a hashmap")]))
   (wrap-binary 'contains? (match-lambda**
                            [((mal-hash _ _ m) k) (hash-has-key? m k)]
                            [(_ _) (raise-mal "contains takes a hashmap and a key value")]))
   (wrap-binary 'get (match-lambda**
                      [((mal-hash _ _ m) (? mal-hashkey? k)) (hash-ref m k (λ () (mal-nil)))]
                      [((mal-nil) _) (mal-nil)]
                      [(_ _) (raise-mal "get takes a hashmap and a key value")]))
   (wrap-general 'assoc (match-lambda
                          [(list (mal-hash _ _ m) args ...)
                           (define new-pairs : (Immutable-HashTable MalHashKey Mal)
                             (flat-list->mal-hashmap args))
                           (mal-hash #f #f
                            (for/fold ([updated-m : (Immutable-HashTable MalHashKey Mal) m])
                                      ([(k v) new-pairs])
                              (hash-set updated-m k v)))]
                          [_ (raise-mal "first argument of assoc must be a hashmap")]))
   (wrap-general 'dissoc (match-lambda
                           [(list (mal-hash _ _ m) keys ...)
                            (mal-hash #f #f
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
                          [(list (? box? b) (mal-function _ _ f) other-args ...)
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
                      [((mal-function _ _ f) (mal-list _ _ xs))
                       (define (f1 [x : Mal]) ; convert to Mal->Mal function
                         (f (list x)))
                       (mal-list #f #f (map f1 xs))]
                      [((mal-function _ _ f) (mal-vector _ _ v))
                       (define (f1 [x : Mal])
                         (f (list x)))
                       (mal-list #f #f (map f1 (vector->list v)))]
                      [(_ _)
                       (raise-mal "bad arguments to map")]))
   (wrap-general 'apply (lambda ([args : (Listof Mal)])
                          (when (< (length args) 2)
                            (raise-mal "need two args for apply"))
                          (define apply-fn : (-> (Listof Mal) Mal)
                            (match (car args)
                              [(mal-function _ _ f) f]
                              [_ (raise-mal "bad first arg for apply")]))                                       
                          (define apply-args : (Listof Mal)
                            (append (drop-right (cdr args) 1) ; args without first and last
                                    (match (last args)
                                      [(mal-list _ _ xs) xs]
                                      [(mal-vector _ _ v) (vector->list v)]
                                      [_ (raise-mal "bad last arg for apply")])))
                          (apply-fn apply-args)))
   (wrap-unary 'meta (match-lambda
                       [(mal-list #f _ _) (mal-nil)]
                       [(mal-vector #f _ _) (mal-nil)]
                       [(mal-hash #f _ _) (mal-nil)]
                       [(mal-function #f _ _) (mal-nil)]
                       [(mal-list _ x _) x]
                       [(mal-vector _ x _) x]
                       [(mal-hash _ x _) x]
                       [(mal-function _ x _) x]
                       [_ (mal-nil)]))
   (wrap-binary 'with-meta (match-lambda**
                             [((mal-list _ _ xs) x) (mal-list #t x xs)]
                             [((mal-vector _ _ v) x) (mal-vector #t x v)]
                             [((mal-hash _ _ m) x) (mal-hash #t x m)]
                             [((mal-function _ _ f) x) (mal-function #t x f)]
                             [(_ _) (raise-mal "can only apply with-meta to a collection or function")]))))
