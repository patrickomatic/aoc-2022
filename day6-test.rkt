#lang racket

(require rackunit "day6.rkt")

(test-case "all-different?"
  (check-equal? (all-different? '(1 2 3 4)) #t)
  (check-equal? (all-different? '(2 2 3 4)) #f)
  (check-equal? (all-different? '(2 2 3 2)) #f)
  (check-equal? (all-different? '(1 2 3 4 5 6 7)) #t))

(test-case "find-marker"
  (check-equal? (find-marker (open-input-string "bvwbjplbgvbhsrlpgdmjqwftvncz") 4) 5)
  (check-equal? (find-marker (open-input-string "nppdvjthqldpwncqszvftbrmjlhg") 4) 6)
  (check-equal? (find-marker (open-input-string "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 4) 10)
  (check-equal? (find-marker (open-input-string "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 4) 11)
  (check-equal? (find-marker (open-input-string "mjqjpqmgbljsphdztnvjfqwrcgsmlb") 14) 19)
  (check-equal? (find-marker (open-input-string "bvwbjplbgvbhsrlpgdmjqwftvncz") 14) 23)
  (check-equal? (find-marker (open-input-string "nppdvjthqldpwncqszvftbrmjlhg") 14) 23)
  (check-equal? (find-marker (open-input-string "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") 14) 29)
  (check-equal? (find-marker (open-input-string "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 14) 26))
