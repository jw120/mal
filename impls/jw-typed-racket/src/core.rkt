#lang typed/racket

(provide core_ns)

(require "printer.rkt" "reader.rkt" "types.rkt")

;(require/typed readline/readline
;               [readline (-> String (U String EOF))])


; function for mal = (version of equal? that treats lists and vectors as equivalent)
#|
(define (mal-equal? x y)
  (match (list x y)
    [(list '() '()) #t]
    [(list '() (cons _ _)) #f]
    [(list (cons _ _) '())  #f]
    [(list (cons xh xt) (cons yh yt)) (and (mal-equal? xh yh) (mal-equal? xt yt))]
    [_ (cond
         [(and (list-or-vector? x) (list-or-vector? y))
          (mal-equal? (list-or-vector->list x) (list-or-vector->list y))]
         [(and (hash? x) (hash? y))
          (and
           (equal? (hash-count x) (hash-count y))
           (for/and ([k (hash-keys x)])
             (and
              (hash-has-key? y k)
              (mal-equal? (hash-ref x k) (hash-ref y k)))))]
         [else (equal? x y)])]))
|#

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

;; Wrap a (-> Mal Boolean) function for inclusion in core_ns
(define (wrap-is [f-sym : Symbol] [f : (-> Mal Boolean)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list x) (f x)]
        [_ (raise-mal (string-append "Expected one argument to " (symbol->string f-sym)))])))))

;; Wrap a (-> String Mal) function for inclusion in core_ns
(define (wrap-str [f-sym : Symbol] [f : (-> String Mal)]) : (Pair Symbol Mal)
  (cons
   f-sym
   (mal-function
    (lambda ([xs : (Listof Mal)])
      (match xs
        [(list (? string? s)) (f s)]
        [_ (raise-mal (string-append "Expected one string argument to " (symbol->string f-sym)))])))))

;; Wrap a (-> (Listof Mal) Mal) function for inclusion in core_ns
(define (wrap-list [f-sym : Symbol] [f : (-> (Listof Mal) Mal)]) : (Pair Symbol Mal)
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

   ;; Equality
   (wrap-binary '= mal-equal?)
   
   ;; Sequence
   (wrap-list 'list mal-list)
   (wrap-list 'count (lambda ([params : (Listof Mal)])
                       (match params
                         [(list (mal-list (list xs ...)) _ ...) (length xs)]
                         [(list (mal-vector v) _ ...) (vector-length v)]
                         [(list (mal-nil) _ ...) 0]
                         [_ (raise-mal "Expected a list for count")])))
   (wrap-is 'list? mal-list?)
   (wrap-is 'empty? (lambda (x)
                      (or
                       (and (mal-list? x) (empty? (mal-list-xs x)))
                       ; vector-empty? does not seem to work in racket/typed
                       (and (mal-vector? x) (equal? 0 (vector-length (mal-vector-v x)))))))

   ;; IO
   (wrap-str 'read-string read_str)
   (wrap-str 'slurp file->string)
   (wrap-list 'prn (lambda ([args : (Listof Mal)])
                     (let ([s : String (string-join (map (lambda ([x : Mal]) (pr_str x #t)) args) " ")])
                       (displayln s)
                       (mal-nil))))
   (wrap-list 'println (lambda ([args : (Listof Mal)])
                         (let ([s : String (string-join (map (lambda ([x : Mal]) (pr_str x #f)) args) " ")])
                           (displayln s)
                           (mal-nil))))
   (wrap-list 'pr-str (lambda ([args : (Listof Mal)])
                        (string-join (map (lambda ([x : Mal]) (pr_str x #t)) args) " ")))
   (wrap-list 'str (lambda ([args : (Listof Mal)])
                     (string-join (map (lambda ([x : Mal]) (pr_str x #f)) args) "")))


   ))




#|
   (cons '< <)
   (cons '> >)
   (cons '<= <=)
   (cons '>= >=)
   (cons '= mal-equal?)



   ; Sequence
   (cons 'list list)
   (cons 'list? list?)
   (cons 'empty? (lambda (x) (if (vector? x) (vector-empty? x) (empty? x))))
   (cons 'count (lambda (x)
                  (cond
                    [(vector? x) (vector-length x)]
                    [(nil? x) 0]
                    [else (length x)])))
   (cons 'cons (lambda (x y) (cons x (list-or-vector->list y))))
   (cons 'concat (lambda args (apply append (map list-or-vector->list args))))
   (cons 'nth (lambda (s n)
                (cond
                  [(vector? s)
                   (when (or (< n 0) (>= n (vector-length s)))
                     (raise-mal-eval "Vector nth index out of bounds"))
                   (vector-ref s n)]
                  [(list? s)
                   (when (or (< n 0) (>= n (length s)))
                     (raise-mal-eval "List nth index out of bounds"))
                   (list-ref s n)]
                  [else (raise-mal-eval "Bad argument to nth")])))
   (cons 'first (lambda (s)
                  (cond
                    [(nil? s) nil]
                    [(vector? s) (if (vector-empty? s) nil (vector-ref s 0))]
                    [(list? s) (if (null? s) nil (first s))]
                    [else (raise-mal-eval "Bad argument to first")])))
   (cons 'rest  (lambda (s)
                  (cond
                    [(nil? s) '()]
                    [(vector? s) (if (vector-empty? s) '() (vector->list (vector-drop s 1)))]
                    [(list? s) (if (null? s) '() (cdr s))]
                    [else (raise-mal-eval "Bad argument to rest")])))
   (cons 'vec (lambda (s)
                  (cond
                    [(vector? s) s]
                    [(list? s) (apply vector-immutable s)]
                    [else (raise-mal-eval "Bad argument to vec")])))
   (cons 'vector vector-immutable)
   (cons 'vector? vector?)
   (cons 'sequential? list-or-vector?)
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
   (cons 'read-string read_string)
   (cons 'slurp file->string)
   (cons 'readline (lambda (prompt)
                     (let ([s (readline prompt)])
                       (if (eof-object? s) nil s))))
   (cons 'prn (lambda args
                (displayln (string-join (map (lambda (x) (pr_str x #t)) args) " "))
                nil))
   (cons 'println (lambda args
                    (displayln (string-join (map (lambda (x) (pr_str x #f)) args) " "))
                    nil))
   (cons 'str (lambda args
                (string-join (map (lambda (x) (pr_str x #f)) args) "")))
   (cons 'pr-str (lambda args
                   (string-join (map (lambda (x) (pr_str x #t)) args) " ")))

   ; Atoms
   (cons 'atom box)
   (cons 'atom? box?)
   (cons 'deref unbox)
   (cons 'reset! (λ (atom val)
                   (set-box! atom val)
                   val))
   (cons 'swap! (λ (atom f . other-args)
                  (let* ([closure (cond
                                   [(procedure? f) f]
                                   [(func? f) (func-closure f)]
                                   [else (raise-mal-eval "Expected a function for swap!")])]
                        [new-val (apply closure (cons (unbox atom) other-args))])
                         (set-box! atom new-val)
                         new-val)))

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
   (cons 'map (lambda (f xs)
                (define map-proc
                    (cond
                      [(procedure? f) f]
                      [(func? f) (func-closure f)]
                      [else (raise-mal-eval "Bad args for apply")]))
                (map map-proc (list-or-vector->list xs))))
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