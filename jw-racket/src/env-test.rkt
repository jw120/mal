#lang racket
 
(require rackunit
         "exceptions.rkt"
         "env.rkt")

(provide env-tests)

(define env-tests
  (test-suite
   "Tests for env.rkt"

   (let*
       ([top (new env%)]
        [sub (new env% [outer top])])
     (send top set "a" 2)
     (send top set "b" 3)
     (send sub set "a" 5)
     (send sub set "c" 8)

     (check-equal? (send top get "a") 2 "Top top a")
     (check-equal? (send top get "b") 3 "Top top b")
     (check-exn exn:mal:eval? (λ () (send top get "c")) "Top missing")
     (check-equal? (send sub get "a") 5 "Sub sub a")
     (check-equal? (send sub get "b") 3 "Sub top b")
     (check-equal? (send sub get "c") 8 "Sub sub c")

     (check-equal? (send sub find "a") sub "Find sub sub a")
     (check-equal? (send sub find "b") top "Find sub top b")
     (check-equal? (send sub find "c") sub "Find sub sub c")
     (check-equal? (send sub find "d") #f "Find sub sub missing"))))
  