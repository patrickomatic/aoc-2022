#lang typed/racket

(: string->real (-> String Real))
(define (string->real s)
  (let ([p (string->number s)])
    (if p (real-part p) (error "Invalid number in input" s))))

(: ++ (-> Real Real))
(define ++ (curry + 1))

(: -- (-> Real Real))
(define (-- a) (- a 1))

(provide ++ -- string->real)
