#lang racket

(require rackunit "day2.rkt")

(test-case
  "score-for-choice"
  (check-equal?  (score-for-choice 'Rock) 1)
  (check-equal?  (score-for-choice 'Paper) 2)
  (check-equal?  (score-for-choice 'Scissors) 3))
