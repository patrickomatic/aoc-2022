#lang racket
(require rackunit "day13.rkt")

(test-case "packets-ordered?"
           (check-equal? (packets-ordered? (packet '() '())) #t)
           (check-equal? (packets-ordered? (packet '((1) (2 3 4)) '((1) 2 3 4))) #f)
           (check-equal? (packets-ordered? (packet '(5 2) '((4) 3))) #f)
           (check-equal? (packets-ordered? (packet '(1 1 3 1 1) '(1 1 5 1 1))) #t)
           (check-equal? (packets-ordered? (packet '((1) (2 3 4)) '((1) 4))) #t)
           (check-equal? (packets-ordered? (packet '(9) '((8 7 6)))) #f)
           (check-equal? (packets-ordered? (packet '((4 4) 4 4) '((4 4) 4 4 4))) #t)
           (check-equal? (packets-ordered? (packet '(7 7 7 7) '(7 7 7))) #f)
           (check-equal? (packets-ordered? (packet '() '(3))) #t)
           (check-equal? (packets-ordered? (packet '(101) '(301))) #t)
           (check-equal? (packets-ordered? (packet '((())) '(()))) #f)
           (check-equal? (packets-ordered? (packet '(1 (2 (3 (4 (5 6 7)))) 8 9) '(1 (2 (3 (4 (5 6 0)))) 8 9))) #f))
