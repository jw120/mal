#lang racket/base

(displayln "In setup")
(current-prompt-read
 (let ([old-prompt-read (current-prompt-read)])
   (lambda () (display "user") (flush-output) (old-prompt-read))))
