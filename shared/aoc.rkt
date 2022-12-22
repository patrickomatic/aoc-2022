#lang typed/racket
(require/typed advent-of-code
               [find-session (-> String)]
               [fetch-aoc-input (->* (String Number Number) (#:cache Boolean) String)])

(: display-advent-of-code-for-day (-> Number Number (-> String Any) (-> String Any) Void))
(define (display-advent-of-code-for-day year day p1-fn p2-fn)
  (let ([input (fetch-aoc-input (find-session) year day #:cache #t)])
    (printf "[Advent of Code ~s] Day ~s/Part 1: ~s\n" year day (time (p1-fn input)))
    (printf "[Advent of Code ~s] Day ~s/Part 2: ~s\n" year day (time (p2-fn input)))))

(provide display-advent-of-code-for-day) 
