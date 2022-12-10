#lang racket

(require rackunit "day8.rkt")

(define test-grid #(#(3 0 3 7 3)
                    #(2 5 5 1 2)
                    #(6 5 3 3 2)
                    #(3 3 5 4 9)
                    #(3 5 3 9 0)))

(test-case "get-visible-trees"
           (check-equal? (get-visible-trees test-grid)
                         '(0 9 3 5 3 9 5 3 2 3 5 6 2 5 5 2 3 7 3 0 3)))
(test-case "tree-height"
           (check-equal? (tree-height test-grid 0 0) 3)
           (check-equal? (tree-height test-grid 1 1) 5)
           (check-equal? (tree-height test-grid 4 4) 0))


