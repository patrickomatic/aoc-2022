#lang typed/racket
(require "shared/aoc.rkt" "shared/number.rkt")

(define-type Range (Pairof Real Real))
(define-type RangePair (Pairof Range Range))

(define-syntax-rule (mpair fn l)
  `(,(fn (first l)) . ,(fn (second l))))

(: parse-range (-> String Range))
(define (parse-range r)
  (mpair string->real (string-split r "-")))

(: load-range-pairs (-> String (Listof RangePair)))
(define (load-range-pairs input)
  (map (λ ([line : String])
          (mpair parse-range (string-split line ",")))
       (string-split input "\n")))

(: partially-overlapping? (-> RangePair Boolean))
(define (partially-overlapping? range-pair) 
  (let ([a (car range-pair)]
        [b (cdr range-pair)])
    (if (<= (car a) (car b))
      (>= (cdr a) (car b))
      (>= (cdr b) (car a)))))

(: fully-overlaps? (-> Range Range Boolean))
(define (fully-overlaps? a b) 
  (and (<= (car a) (car b)) (>= (cdr a) (cdr b))))

(: either-way (-> (-> Range Range Boolean) RangePair Boolean))
(define (either-way overlap-fn range)
  (or (overlap-fn (car range) (cdr range))
      (overlap-fn (cdr range) (car range))))

(define fully-overlapping? ((curry either-way) fully-overlaps?))

(: q4-part1 (-> String Real))
(define (q4-part1 input)
  (length (filter fully-overlapping? (load-range-pairs input))))

(: q4-part2 (-> String Real))
(define (q4-part2 input)
  (length (filter partially-overlapping? (load-range-pairs input))))

(display-advent-of-code-for-day 2022 4 q4-part1 q4-part2)

(provide fully-overlapping? partially-overlapping?)
