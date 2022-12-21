#lang racket
(require rackunit 
         "day15.rkt" 
         "shared/sparse-matrix.rkt" 
         "shared/grid.rkt")

(define (test-grid) (make-sparse-matrix))

(test-case "q15-part1"
           (check-equal? (q15-part1 10
                                    (list
                                      (sensor '(2 . 18) '(-2 . 15))
                                      (sensor '(9 . 16) '(10 . 16))
                                      (sensor '(13 . 2) '(15 . 3))
                                      (sensor '(12 . 14) '(10 . 16))
                                      (sensor '(10 . 20) '(10 . 16))
                                      (sensor '(14 . 17) '(10 . 16))
                                      (sensor '(8 . 7) '(2 . 10))
                                      (sensor '(2 . 0) '(2 . 10))
                                      (sensor '(0 . 11) '(2 . 10))
                                      (sensor '(20 . 14) '(25 . 17))
                                      (sensor '(17 . 20) '(21 . 22))
                                      (sensor '(16 . 7) '(15 . 3))
                                      (sensor '(14 . 3) '(15 . 3))
                                      (sensor '(20 . 1) '(15 . 3))))
                         26))

#|
(test-case "fill-in-sensors!"
           (check-equal? (let* ([output (open-output-string)]
                               [sensors (list (sensor '(8 . 7) '(2 . 10)))]
                               [filled-in (fill-in-sensors! sensors)])
                           (display-sparse-matrix output filled-in '(0 . 0) 23 17 sensors)
                           (get-output-string output))
                         #<<EOF

.......###........
......#####.......
.....#######......
....#########.....
...###########....
..#############...
.###############..
#################.
.###############..
..#############...
...###########....
....#########.....
.....#######......
......#####.......
.......###........
........#.........
..................

EOF
))
|#
