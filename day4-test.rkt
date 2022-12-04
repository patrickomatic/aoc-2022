#lang racket

(require rackunit "day4.rkt")

(test-case
  "fully-overlapping?"
  (check-equal?  (fully-overlapping? '((2 . 4) . (6 . 8))) #f)
  (check-equal?  (fully-overlapping? '((2 . 3) . (4 . 5))) #f)
  (check-equal?  (fully-overlapping? '((5 . 7) . (7 . 9))) #f)
  (check-equal?  (fully-overlapping? '((2 . 8) . (3 . 7))) #t)
  (check-equal?  (fully-overlapping? '((6 . 6) . (4 . 6))) #t)
  (check-equal?  (fully-overlapping? '((2 . 6) . (4 . 8))) #f))
