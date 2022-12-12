#lang racket

(require rackunit "day10.rkt")

(define test-instructions '(
                            (addx . 15) (addx . -11) (addx . 6) (addx . -3) (addx . 5) (addx . -1) 
                            (addx . -8) (addx . 13) (addx . 4) (noop) (addx . -1) (addx . 5) (addx . -1)
                            (addx . 5) (addx . -1) (addx . 5) (addx . -1) (addx . 5) (addx . -1) (addx . -35)
                            (addx . 1) (addx . 24) (addx . -19) (addx . 1) (addx . 16) (addx . -11) (noop)
                            (noop) (addx . 21) (addx . -15) (noop) (noop) (addx . -3) (addx . 9) (addx . 1)
                            (addx . -3) (addx . 8) (addx . 1) (addx . 5) (noop) (noop) (noop) (noop) (noop)
                            (addx . -36) (noop) (addx . 1) (addx . 7) (noop) (noop) (noop) (addx . 2) (addx . 6)
                            (noop) (noop) (noop) (noop) (noop) (addx . 1) (noop) (noop) (addx . 7) (addx . 1) 
                            (noop) (addx . -13) (addx . 13) (addx . 7) (noop) (addx . 1) (addx . -33) (noop)
                            (noop) (noop) (addx . 2) (noop) (noop) (noop) (addx . 8) (noop) (addx . -1) (addx . 2)
                            (addx . 1) (noop) (addx . 17) (addx . -9) (addx . 1) (addx . 1) (addx . -3) (addx . 11)
                            (noop) (noop) (addx . 1) (noop) (addx . 1) (noop) (noop) (addx . -13) (addx . -19)
                            (addx . 1) (addx . 3) (addx . 26) (addx . -30) (addx . 12) (addx . -1) (addx . 3)
                            (addx . 1) (noop) (noop) (noop) (addx . -9) (addx . 18) (addx . 1) (addx . 2) (noop)
                            (noop) (addx . 9) (noop) (noop) (noop) (addx . -1) (addx . 2) (addx . -37) (addx . 1)
                            (addx . 3) (noop) (addx . 15) (addx . -21) (addx . 22) (addx . -6) (addx . 1) (noop)
                            (addx . 2) (addx . 1) (noop) (addx . -10) (noop) (noop) (addx . 20) (addx . 1)
                            (addx . 2) (addx . 2) (addx . -6) (addx . -11) (noop) (noop) (noop)))

(test-case "fill-pipeline"
           (check-equal? (fill-pipeline '((noop) (addx . 1) (noop) (addx . 2)))
                         '(noop noop 1 noop noop 2)))
(test-case "run-cpu!"
           (check-equal? (run-cpu! test-instructions '(20 60 100 140 180 220)) 
                         '(3960 2880 2940 1800 1140 420)))

(test-case "sprite-overlaps-row-index?"
           (check-equal? (sprite-overlaps-row-index? 0 0) #t)
           (check-equal? (sprite-overlaps-row-index? 0 5) #f)
           (check-equal? (sprite-overlaps-row-index? 2 4) #f)
           (check-equal? (sprite-overlaps-row-index? 3 4) #t)
           (check-equal? (sprite-overlaps-row-index? 4 4) #t)
           (check-equal? (sprite-overlaps-row-index? 5 4) #t))
