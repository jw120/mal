#lang typed/racket

(provide (all-defined-out))

(require "types.rkt")

;; Functions that wrap simpler functions for use in the core_ns list

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
(define (wrap-unary-str [f-sym : Symbol] [f : (-> String Mal)]) : (Pair Symbol Mal)
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
