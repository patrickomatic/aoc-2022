#lang racket

(require rackunit "day12.rkt")

(define test-heightmap (heightmap '(0 . 0) '(5 . 2) `#(#(,@(string->list "Sabqponm"))
                                                       #(,@(string->list "abcryxxl"))
                                                       #(,@(string->list "accszExk"))
                                                       #(,@(string->list "acctuvwj"))
                                                       #(,@(string->list "abdefghi"))) '()))
    

(test-case "bfs"
           (check-equal? (heightmap-shortest-path (bfs test-heightmap '(0 . 0))) '(
                                                         (0 . 0) (0 . 1) (1 . 1) (1 . 2)
                                                         (1 . 3) (2 . 3) (2 . 4) (3 . 4)
                                                         (4 . 4) (5 . 4) (6 . 4) (7 . 4)
                                                         (7 . 3) (7 . 2) (7 . 1) (7 . 0)
                                                         (6 . 0) (5 . 0) (4 . 0) (3 . 0)
                                                         (3 . 1) (3 . 2) (3 . 3) (4 . 3)
                                                         (5 . 3) (6 . 3) (6 . 2) (6 . 1)
                                                         (5 . 1) (4 . 1) (4 . 2) (5 . 2))))

(test-case "can-handle-elevation-change?"
           (check-equal? (can-handle-elevation-change? test-heightmap '(0 . 1) '(0 . 2)) #t)
           (check-equal? (can-handle-elevation-change? test-heightmap '(1 . 0) '(2 . 0)) #t)
           (check-equal? (can-handle-elevation-change? test-heightmap '(3 . 2) '(4 . 2)) #f)
           (check-equal? (can-handle-elevation-change? test-heightmap '(4 . 2) '(3 . 2)) #t))

(test-case "find-marker" 
           (check-equal? (find-marker (heightmap-elevations test-heightmap) #\S) '(0 . 0)))

(test-case "get-adjacent-steps"
           (check-equal? (get-adjacent-steps test-heightmap '(0 . 0)) '((0 . 1) (1 . 0)))
           (check-equal? (get-adjacent-steps test-heightmap '(1 . 1)) '((1 . 2) (2 . 1) (1 . 0) (0 . 1)))
           (check-equal? (get-adjacent-steps test-heightmap '(4 . 2)) '((4 . 3) (5 . 2) (4 . 1) (3 . 2))))

(test-case "within-bounds?" 
           (check-equal? (within-bounds? test-heightmap '(0 . 0)) #t)
           (check-equal? (within-bounds? test-heightmap '(0 . 1)) #t)
           (check-equal? (within-bounds? test-heightmap '(-1 . 5)) #f)
           (check-equal? (within-bounds? test-heightmap '(0 . -1)) #f)
           (check-equal? (within-bounds? test-heightmap '(100 . 0)) #f)
           (check-equal? (within-bounds? test-heightmap '(4 . 4)) #t)
           (check-equal? (within-bounds? test-heightmap '(2 . 3)) #t))
