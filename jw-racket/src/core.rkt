#lang racket

(provide ns)

(require "printer.rkt" "utils.rkt")

(define ns
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
   (cons '< <)
   (cons '> >)
   (cons '<= <=)
   (cons '>= >=)
   (cons '= equal?)
   (cons 'list list)
   (cons 'list? list?)
   (cons 'empty? empty?)
   (cons 'count (lambda (x)
                  (cond
                    [(vector? x) (vector-length x)]
                    [(nil? x) 0]
                    [else (length x)])))
   (cons 'prn (lambda args
                (display (string-join (map (lambda (x) (pr_str x #t)) args) " "))
                nil))
   (cons 'println (lambda args
                    (display (string-join (map (lambda (x) (pr_str x #f)) args) " "))
                    nil))
   (cons 'str (lambda args
                (string-join (map (lambda (x) (pr_str x #f)) args) "")))
   (cons 'pr-str (lambda args
                (string-join (map (lambda (x) (pr_str x #t)) args) " ")))
   ))
