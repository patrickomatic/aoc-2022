#lang racket

(require rackunit "day9.rkt")

(define (pr r) (format "~s" r))

(test-case "rope write-proc"
           (check-equal? (pr (rope '((2 . 2) (1 . 1) (0 . 0)) '((0 . 0)))) #<<eof
2.....
.1....
..H...
......
......
......

eof
))

(test-case "move-knots"
           (check-equal? (pr (move-knots short-rope)) #<<eof
H.....
......
......
......
......
......

eof
)
           (check-equal? (pr (move-knots (rope '((2 . 0) (0 . 0)) '((0 . 0))))) #<<eof
.1H...
......
......
......
......
......

eof
)
           (check-equal? (pr (move-knots (rope '((2 . 2) (0 . 0)) '((0 . 0))))) #<<eof
......
.1....
..H...
......
......
......

eof
))

(test-case "points-adjacent?"
           (check-equal? (points-adjacent? '(0 . 0) '(0 . 0)) #t)
           (check-equal? (points-adjacent? '(0 . 0) '(0 . 1)) #t)
           (check-equal? (points-adjacent? '(0 . 0) '(1 . 0)) #t)
           (check-equal? (points-adjacent? '(0 . 0) '(1 . 1)) #t)
           (check-equal? (points-adjacent? '(0 . 0) '(2 . 2)) #f)
           (check-equal? (points-adjacent? '(0 . 0) '(0 . 2)) #f)
           (check-equal? (points-adjacent? '(0 . 0) '(2 . 0)) #f))

(test-case "run-motions"
           (check-equal? (pr (run-motions '((R . 4)) short-rope)) #<<eof
...1H.
......
......
......
......
......

eof
)
           (check-equal? (pr (run-motions '((R . 4) (U . 4)) short-rope)) #<<eof
......
......
......
....1.
....H.
......

eof
)
           (check-equal? (pr (run-motions '((R . 4) (U . 4) (L . 3)) short-rope)) #<<eof
......
......
......
......
.H1...
......

eof
)
           (check-equal? (pr (run-motions '((R . 4) (U . 4) (L . 3) (D . 1)) short-rope)) #<<eof
......
......
......
.H....
..1...
......

eof
)
           (check-equal? (pr (run-motions '((R . 4) (U . 4) (L . 3) (D . 1) (R . 4)) short-rope)) #<<eof
......
......
......
....1H
......
......

eof
)
           (check-equal? (pr (run-motions '((R . 4) (U . 4) (L . 3) (D . 1) (R . 4) (D . 1)) short-rope)) #<<eof
......
......
.....H
....1.
......
......

eof
)
           (check-equal? (pr (run-motions '((R . 1)) long-rope)) #<<eof
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
......................
....................1H
......................

eof
)
           (check-equal? (pr (run-motions '((R . 2)) long-rope)) #<<eof
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
.......................
....................21H
.......................
.......................

eof
)

           (check-equal? (pr (run-motions '((R . 3)) long-rope)) #<<eof
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
........................
....................321H
........................
........................
........................

eof
)
           (check-equal? (pr (run-motions '((R . 4)) long-rope)) #<<eof
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
.........................
....................4321H
.........................
.........................
.........................
.........................

eof
)
           (check-equal? (pr (run-motions '((R . 5)) long-rope)) #<<eof
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
..........................
....................54321H
..........................
..........................
..........................
..........................
..........................

eof
)

           (check-equal? (pr (run-motions '((R . 5) 
                                            (U . 8)
                                            (L . 8)
                                            (D . 3)
                                            (R . 17)
                                            (D . 10)
                                            (L . 25)
                                            (U . 20)) 
                                          long-rope)) #<<eof
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
....................................
.........9..........................
.........8..........................
.........7..........................
.........6..........................
.........5..........................
.........4..........................
.........3..........................
.........2..........................
.........1..........................
.........H..........................

eof
))