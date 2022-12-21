#lang typed/racket

(: string->real (-> String Real))
(define (string->real s)
  (let ([p (string->number s)])
    (if p (real-part p) (error "Invalid number in input" s))))

(provide string->real)
