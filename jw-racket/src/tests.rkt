#lang racket/base
 
(require rackunit
         "env-test.rkt"
         "reader-test.rkt"
         "utils-test.rkt")

(require rackunit/text-ui)

(define (run tests label)
  (printf "~a: " label)
  (run-tests tests))

(define number-of-failed-tests
  (+
   (run env-tests "env")
   (run reader-tests "reader")
   (run utils-tests "utils")))

(exit (if (equal? number-of-failed-tests 0) 0 1))
