#lang racket

(require rackunit "day1.rkt")

(test-case
  "group-calories"
  (check-equal?
    (group-calories (list 1000 500 'Separator 200 'Separator 100 3000 4000))
    (list 1500 200 7100))
  (check-equal?
    (group-calories '())
    '(0)))

(test-case
  "parse-input"
  (check-equal?
    (parse-input (list "100" "200" "" "300"))
    (list 100 200 'Separator 300))
  (check-equal?
    (parse-input (list ""))
    '(Separator)))
