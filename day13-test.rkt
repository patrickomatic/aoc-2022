#lang racket
(require rackunit "day13.rkt")

(test-case "packets-ordered?"
           (check-equal? (packets-ordered? '() '()) #t)
           (check-equal? (packets-ordered? '((1) (2 3 4)) '((1) 2 3 4)) #f)
           (check-equal? (packets-ordered? '(5 2) '((4) 3)) #f)
           (check-equal? (packets-ordered? '(1 1 3 1 1) '(1 1 5 1 1)) #t)
           (check-equal? (packets-ordered? '((1) (2 3 4)) '((1) 4)) #t)
           (check-equal? (packets-ordered? '(9) '((8 7 6))) #f)
           (check-equal? (packets-ordered? '((4 4) 4 4) '((4 4) 4 4 4)) #t)
           (check-equal? (packets-ordered? '(7 7 7 7) '(7 7 7)) #f)
           (check-equal? (packets-ordered? '() '(3)) #t)
           (check-equal? (packets-ordered? '(101) '(301)) #t)
           (check-equal? (packets-ordered? '((())) '(())) #f)
           (check-equal? (packets-ordered? '(1 (2 (3 (4 (5 6 7)))) 8 9) '(1 (2 (3 (4 (5 6 0)))) 8 9)) #f))
