#lang racket
(require rackunit "number.rkt")

(test-case "string->real"
           (check-equal? (string->real "595") 595)
           (check-exn exn:fail? (λ () (string->real "foo"))))
