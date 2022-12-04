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

(: range-contains? (-> Range Range Boolean))
(define (range-contains? a b)
  (and (<= (car a) (car b)) (>= (cdr a) (cdr b))))

(: fully-overlapping? (-> RangePair Boolean))
(define (fully-overlapping? range)
  (or (range-contains? (car range) (cdr range))
      (range-contains? (cdr range) (car range))))

(: q4-part1 (-> (Listof RangePair) Real))
(define (q4-part1 range-pairs)
  (length (filter fully-overlapping? range-pairs)))

(let ([range-pairs (load-range-pairs "input/day4.txt")])
  (printf "Question 4/Part 1: ~a\n" (q4-part1 range-pairs))
  (printf "Question 4/Part 2: ~a\n" (first range-pairs)))

(provide fully-overlapping?)
