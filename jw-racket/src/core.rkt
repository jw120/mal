#lang racket

(provide ns)

(require "printer.rkt" "utils.rkt")

; function for mal = (version of equal? that treats lists and vectors as equivalent)
(define (mal-equal? x y)
  (match (list x y)
    [(list '() '()) #t]
    [(list '() (cons _ _)) #f]
    [(list (cons _ _) '())  #f]
    [(list (cons xh xt) (cons yh yt)) (and (mal-equal? xh yh) (mal-equal? xt yt))]
    [_ (cond
            [(and (list-or-vector? x) (list-or-vector? y)) (mal-equal? (list-or-vector->list x) (list-or-vector->list y))]
            [else (equal? x y)])]))

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
   (cons '= mal-equal?)
   (cons 'list list)
   (cons 'list? list?)
   (cons 'empty? (lambda (x) (if (vector? x) (vector-empty? x) (empty? x))))
   (cons 'count (lambda (x)
                  (cond
                    [(vector? x) (vector-length x)]
                    [(nil? x) 0]
                    [else (length x)])))
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
   ))
