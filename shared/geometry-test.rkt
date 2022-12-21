#lang racket
(require rackunit "geometry.rkt")

(test-case "clip-coord"
           (check-equal? (clip-coord 5) 5)
           (check-equal? (clip-coord 1) 1)
           (check-equal? (clip-coord 0) 0)
           (check-equal? (clip-coord -1) 0)
           (check-equal? (clip-coord -100) 0)
           (check-equal? (clip-coord 5 #:clip-at 10) 10))

(test-case "clip-point"
           (check-equal? (clip-point `(0 . 0)) '(0 . 0))
           (check-equal? (clip-point `(-1 . -1)) '(0 . 0))
           (check-equal? (clip-point `(-20 . 5)) '(0 . 5))
           (check-equal? (clip-point `(5 . 5)) '(5 . 5))
           (check-equal? (clip-point `(25 . 8) #:clip-at-x 30 #:clip-at-y 10) '(30 . 10)))
