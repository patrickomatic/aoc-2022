#lang typed/racket

(define-type Range (Pairof Real Real))
(define-type RangePair (Pairof Range Range))

(: parse-integer (-> String Real))
(define (parse-integer s)
  (let ([p (string->number s)])
    (if p (real-part p) (error "Invalid number in input" s))))

(define-syntax-rule (mpair fn l)
  (cons (fn (first l)) (fn (second l))))

(: parse-range (-> String Range))
(define (parse-range r)
  (mpair parse-integer (string-split r "-")))

(: load-range-pairs (-> String (Listof RangePair)))
(define (load-range-pairs filename)
  (map (λ ([line : String])
          (mpair parse-range (string-split line ",")))
       (file->lines filename)))

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

(: q4-part1 (-> (Listof RangePair) Real))
(define (q4-part1 range-pairs)
  (length (filter fully-overlapping? range-pairs)))

(: q4-part2 (-> (Listof RangePair) Real))
(define (q4-part2 range-pairs)
  (length (filter partially-overlapping? range-pairs)))

(let ([range-pairs (load-range-pairs "input/day4.txt")])
  (printf "Question 4/Part 1: ~a\n" (q4-part1 range-pairs))
  (printf "Question 4/Part 2: ~a\n" (q4-part2 range-pairs)))

(provide fully-overlapping? partially-overlapping?)
