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
  (check-equal? (overlapping-chars "abc123" (list "123")) (list #\1 #\2 #\3))
  (check-equal? (overlapping-chars "1111111" (list "123")) (list #\1))
  (check-equal? (overlapping-chars "1111111" (list "123" "100" "000")) '())
  (check-equal? (overlapping-chars "1111111" (list "123" "100" "1")) (list #\1))
  (check-equal? (overlapping-chars "abc" (list "def")) '()))

(test-case
  "item-overlap-score"
  (check-equal? (item-overlap-score "vJrwpWtwJgWrhcsFMMfFFhFp") 16)
  (check-equal? (item-overlap-score "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL") 38)
  (check-equal? (item-overlap-score "PmmdzqPrVvPwwTWBwg") 42)
  (check-equal? (item-overlap-score "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn") 22)
  (check-equal? (item-overlap-score "ttgJtRGJQctTZtZT") 20)
  (check-equal? (item-overlap-score "CrZsJsPPZsGzwwsLwLmpwMDw") 19))

(test-case
  "group-items"
  (check-equal? (group-items (list "a" "b" "c" "d" "e" "f")) 
                (list (list "a" "b" "c") (list "d" "e" "f"))))

(test-case
  "group-overlap-score"
  (check-equal? (group-overlap-score (list "vJrwpWtwJgWrhcsFMMfFFhFp"
                                           "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                                           "PmmdzqPrVvPwwTWBwg"))
                18)
  (check-equal? (group-overlap-score (list "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                                           "ttgJtRGJQctTZtZT"
                                           "CrZsJsPPZsGzwwsLwLmpwMDw"))
                52))
