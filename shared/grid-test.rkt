#lang racket
(require rackunit "grid.rkt")

(define test-grid (grid->mutable-grid `#(#(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9)
                                         #(0 1 2 3 4 5 6 7 8 9))))

(test-case "scale-grid"
           (check-equal? (sub-grid test-grid '(2 . 2) 5 5)
                         `#(#(2 3 4 5 6)
                            #(2 3 4 5 6)
                            #(2 3 4 5 6)
                            #(2 3 4 5 6)
                            #(2 3 4 5 6))))
