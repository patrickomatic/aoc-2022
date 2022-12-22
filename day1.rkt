#lang typed/racket
(require "shared/aoc.rkt")

(define-type Calories Real)
(define-type ParsedEntry (U Calories 'Separator))

(: group-calories (-> (Listof ParsedEntry) (Listof Calories)))
(define (group-calories entries)
  (foldr
   (λ ([v : ParsedEntry] [current : (Listof Calories)])
     (if (eq? v 'Separator)
         (cons 0 current)
         (cons (+ v (car current)) (cdr current))))
   '(0)
   entries))
   
(: parse-input (-> (Listof String) (Listof ParsedEntry)))
(define (parse-input lines)
  (map
   (λ ([s : String])
     (let ([parsed-number (string->number s 10)])
       (if parsed-number
           (real-part parsed-number)
           'Separator)))
   lines))

(: rank-calories (-> (Listof String) (Listof Real)))
(define (rank-calories lines)
  (sort (group-calories (parse-input lines)) >))

(: q1-part1 (-> String Calories))
(define (q1-part1 lines)
  (first (rank-calories (string-split lines "\n"))))
   
(: q1-part2 (-> String Calories))
(define (q1-part2 lines)
   (apply + (take (rank-calories (string-split lines "\n")) 3)))

(display-advent-of-code-for-day 2022 1 q1-part1 q1-part2)
(provide group-calories parse-input rank-calories)
