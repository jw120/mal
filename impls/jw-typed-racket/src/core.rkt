#lang typed/racket

(provide core_ns)

(require "printer.rkt" "reader.rkt" "types.rkt")

;(require/typed readline/readline
;               [readline (-> String (U String EOF))])


;; Wrap a (-> Integer Integer Mal) function for inclusion in core_ns
(define (wrap-binary-int [f-sym : Symbol] [f : (-> Integer Integer Mal)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list (? exact-integer? x) (? exact-integer? y)) (f x y)]
        [_ (raise-mal (string-append "Expecting two numbers as argument to " (symbol->string f-sym)))])))))

;; Wrap a (-> Mal Mal Mal) function for inclusion in core_ns
(define (wrap-binary [f-sym : Symbol] [f : (-> Mal Mal Mal)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list x y) (f x y)]
        [_ (raise-mal (string-append "Expecting two arguments to " (symbol->string f-sym)))])))))

;; Wrap a (-> Mal Mal) function for inclusion in core_ns
(define (wrap-unary [f-sym : Symbol] [f : (-> Mal Mal)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list x) (f x)]
        [_ (raise-mal (string-append "Expecting one arguments to " (symbol->string f-sym)))])))))

;; Wrap a (-> String Mal) function for inclusion in core_ns
(define (wrap-str [f-sym : Symbol] [f : (-> String Mal)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list (? string? s)) (f s)]
        [_ (raise-mal (string-append "Expecting a string as argument to " (symbol->string f-sym)))])))))


;; Wrap a (-> Mal Boolean) function for inclusion in core_ns
(define (wrap-is [f-sym : Symbol] [f : (-> Mal Boolean)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list x) (f x)]
        [_ (raise-mal (string-append "Expected one argument to " (symbol->string f-sym)))])))))

;; Wrap a general function that takes the underlying list of arguments for inclusion in core_ns
(define (wrap-general [f-sym : Symbol] [f : (-> (Listof Mal) Mal)]) : (Pair Symbol Mal)
  (cons f-sym (mal-function f)))

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

   ;; Arithmetic and logical
   (wrap-binary-int '+ +)
   (wrap-binary-int '- -)
   (wrap-binary-int '* *)
   (wrap-binary-int '/ quotient)
   (wrap-binary-int '< <)
   (wrap-binary-int '> >)
   (wrap-binary-int '<= <=)
   (wrap-binary-int '>= >=)

   ;; Equality and is?
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
   
   ;; Sequence
   (wrap-general 'list mal-list)
   (wrap-general 'count (lambda ([params : (Listof Mal)])
                          (match params
                            [(list (mal-list (list xs ...)) _ ...) (length xs)]
                            [(list (mal-vector v) _ ...) (vector-length v)]
                            [(list (mal-nil) _ ...) 0]
                            [_ (raise-mal "Expected a list for count")])))

   (wrap-is 'empty? (lambda ([x : Mal])
                      (or
                       (and (mal-list? x) (empty? (mal-list-xs x)))
                       ; vector-empty? does not seem to work in racket/typed
                       (and (mal-vector? x) (equal? 0 (vector-length (mal-vector-v x)))))))
   (wrap-general 'cons (lambda ([params : (Listof Mal)])
                         (match params
                           [(list x (mal-list ys)) (mal-list (cons x ys))]
                           [(list x (mal-vector v)) (mal-list (cons x (vector->list v)))])))
   (wrap-general 'concat mal-concat)
   (wrap-general 'vec (lambda ([params : (Listof Mal)])
                        (match params
                          [(list (mal-list xs)) (mal-vector (vector->immutable-vector (list->vector xs)))]
                          [(list (mal-vector v)) (mal-vector v)]
                          [_ (raise-mal "expected list or vector as argument to vec")])))
   (wrap-general 'vector (lambda ([params : (Listof Mal)])
                           (mal-vector (vector->immutable-vector (list->vector params)))))
   (wrap-general 'nth (lambda ([params : (Listof Mal)])
                        (match params
                          [(list (mal-list xs) (? exact-integer? n))
                           (when (or (< n 0) (>= n (length xs)))
                             (raise-mal "nth index out of bounds"))
                           (list-ref xs n)]
                          [(list (mal-vector v) (? exact-integer? n))
                           (when (or (< n 0) (>= n (vector-length v)))
                             (raise-mal "nth index out of bounds"))
                           (vector-ref v n)]
                          [_ (raise-mal "bad arguments to nth")])))
   (wrap-general 'first (lambda ([params : (Listof Mal)])
                          (match params
                            [(list (mal-list xs))
                             (if (null? xs) (mal-nil) (first xs))]
                            [(list (mal-vector v))
                             (if (equal? 0 (vector-length v)) (mal-nil) (vector-ref v 0))]
                            [(list (mal-nil))
                             (mal-nil)]
                            [_
                             (raise-mal "bad arguments to first")])))
   (wrap-general 'rest (lambda ([params : (Listof Mal)])
                         (match params
                           [(list (mal-list xs))
                            (if (null? xs)
                                (mal-list '())
                                (mal-list (cdr xs)))]
                           [(list (mal-vector v))
                            (if (equal? 0 (vector-length v))
                                (mal-list '())
                                (mal-list (vector->list (vector-drop v 1))))]
                           [(list (mal-nil))
                            (mal-list '())]
                           [_
                            (raise-mal "bad arguments to rest")])))
                                  
   ;; IO
   (wrap-str 'read-string read_str)
   (wrap-str 'slurp file->string)
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

   ;; Atoms
 
   (wrap-general 'atom (λ ([args : (Listof Mal)])
                         (match args
                           [(list v) (box v)]
                           [_ (raise-mal "need one value for atom")])))
   (wrap-general 'deref (λ ([args : (Listof Mal)])
                          (match args
                            [(list (box b)) b]
                            [_ (raise-mal "need an atom to deref")])))
   (wrap-general 'reset!  (λ ([args : (Listof Mal)])
                            (match args
                              [(list (? box? b) val)
                               (set-box! b val)
                               val]
                              [_ (raise-mal "need an atom and a value for reset!")])))
   (wrap-general 'swap! (λ ([args : (Listof Mal)])
                          (match args
                            [(list (? box? b) (mal-function f) other-args ...)
                             (let* ([args : (Listof Mal) (cons (unbox b) other-args)]
                                    [new-val : Mal (f args)])
                               (set-box! b new-val)
                               new-val)]
                            [_ (raise-mal "need an atom, a function for swap!")])))

   ;; Misc
   (cons '*host-language* "jw-typed-racket")
   (wrap-str 'symbol string->symbol)
   (wrap-str 'keyword mal-keyword)
   (wrap-general 'throw (lambda ([params : (Listof Mal)])
                          (match params
                            [(list val) (raise-mal val)]
                            [_ (raise-mal "bad arguments to throw")])))

   (wrap-general 'map (lambda ([params : (Listof Mal)])
                        (match params
                          [(list (mal-function f) (mal-list xs))
                           (define (f1 [x : Mal]) ; convert to Mal->Mal function
                             (f (list x)))
                           (mal-list (map f1 xs))]
                          [(list (mal-function f) (mal-vector v))
                           (define (f1 [x : Mal])
                             (f (list x)))
                           (mal-list (map f1 (vector->list v)))]
                          [_
                           (raise-mal "bad arguments to map")])))
         

   ))




#|
  


   (cons 'vector vector-immutable)

   (cons 'seq (lambda (x)
                (cond
                  [(list? x) (if (empty? x) nil x)]
                  [(vector? x) (if (vector-empty? x) nil (vector->list x))]
                  [(non-empty-string? x) (cdr (drop-right (string-split x "") 1))]
                  [(equal? "" x) nil]
                  [(nil? x) nil]
                  [else raise-mal-eval "Bad argument to seq"])))
   (cons 'conj (lambda args
                 (when (< (length args) 2)
                   (raise-mal-eval "Bad arguments to conj"))
                 (define collection (car args))
                 (define adds (cdr args))
                 (cond
                   [(list? collection) (append (reverse adds) collection)]
                   [(vector? collection) (vector-append collection (list->vector adds))]
                   [else (raise-mal-eval "Bad collection type for conj")])))

   ; Hash maps
   (cons 'hash-map hash)
   (cons 'map? hash?)
   (cons 'keys hash-keys)
   (cons 'vals hash-values)
   (cons 'contains? hash-has-key?)
   (cons 'get (lambda (hm k) (if (nil? hm) nil (hash-ref hm k nil))))
   (cons 'assoc (lambda args
                  (hash-union (car args) (apply hash (cdr args)) #:combine/key (lambda (k v1 v2) v2))))
   (cons 'dissoc (lambda args
         (for/fold ([hm (car args)]) ([k (cdr args)]) (hash-remove hm k))))

   ; I/O

   (cons 'readline (lambda (prompt)
                     (let ([s (readline prompt)])
                       (if (eof-object? s) nil s))))
 
   ; Misc
   (cons 'apply (lambda args
                  (when (< (length args) 2)
                    (raise-mal-eval "Bad args for apply"))
                  (define apply-proc
                    (cond
                      [(procedure? (car args)) (car args)]
                      [(func? (car args)) (func-closure (car args))]
                      [else (raise-mal-eval "Bad args for apply")]))
                  (define middle-args (drop-right (cdr args) 1))
                  (define last-arg (last args))
                  (define apply-args (append middle-args
                                             (if (list-or-vector? last-arg)
                                                 (list-or-vector->list last-arg)
                                                 (list last-arg))))
                  (apply apply-proc apply-args)))

   (cons 'throw raise-mal-throw)
   (cons 'nil? nil?)
   (cons 'true? (lambda (x) (equal? x #t)))
   (cons 'false? (lambda (x) (equal? x #f)))
   (cons 'symbol? mal-symbol?)
   (cons 'symbol string->symbol)
   (cons 'keyword? keyword?)
   (cons 'keyword (lambda (x) (if (keyword? x) x (string->keyword x))))
   (cons 'string? string?)
   (cons 'number? number?)
   (cons 'fn? (lambda (x) (or (procedure? x) (and (func? x) (not (func-is-macro? x))))))
   (cons 'macro? (lambda (x) (and (func? x) (func-is-macro? x))))
   (cons '*host-language* "jw-racket")
   (cons 'time-ms (λ args (raise-mal-throw "NYI")))
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