#lang racket

(require rackunit "day3.rkt")

(test-case
  "split-item"
  (check-equal?  (split-item "aaaabbbb") (cons "aaaa" "bbbb"))
  (check-equal?  (split-item "aaaaabbbb") (cons "aaaa" "abbbb")))

(test-case
  "priority"
  (check-equal? (priority #\a) 1)
  (check-equal? (priority #\z) 26)
  (check-equal? (priority #\A) 27)
  (check-equal? (priority #\Z) 52))

(test-case
  "overlapping-chars"
  (check-equal? (overlapping-chars "abc123" "123") (list #\1 #\2 #\3))
  (check-equal? (overlapping-chars "1111111" "123") (list #\1))
  (check-equal? (overlapping-chars "abc" "def") '()))

(test-case
  "overlap-score"
  (check-equal? (overlap-score "vJrwpWtwJgWrhcsFMMfFFhFp") 16)
  (check-equal? (overlap-score "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL") 38)
  (check-equal? (overlap-score "PmmdzqPrVvPwwTWBwg") 42)
  (check-equal? (overlap-score "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn") 22)
  (check-equal? (overlap-score "ttgJtRGJQctTZtZT") 20)
  (check-equal? (overlap-score "CrZsJsPPZsGzwwsLwLmpwMDw") 19))
