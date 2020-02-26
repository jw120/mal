#lang racket/base
 
(require rackunit
         "reader-test.rkt"
         "utils-test.rkt")

(require rackunit/text-ui)

(define number-of-failed-tests
  (+
   (run-tests reader-tests 'verbose)
   (run-tests utils-tests 'verbose)))

(when (equal? number-of-failed-tests 0) (displayln "OK"))
