#lang racket/base

(provide ns)

(require racket/file
         racket/list
         racket/match
         racket/string
         racket/vector
         "printer.rkt"
         "reader.rkt"
         "utils.rkt")

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

   ; Arithmetic
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)
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

   ; I/O
   (cons 'read-string read_string)
   (cons 'slurp file->string)
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
   (cons 'swap! (λ (atom func . other-args)
                       (let ([new-val (apply func (cons (unbox atom) other-args))])
                         (set-box! atom new-val)
                         new-val)))

   ))
