#lang typed/racket
(require "shared/aoc.rkt")

(: all-different? (All (A) (-> (Listof A) Boolean)))
(define (all-different? xs)
  (let ([exists-once? (λ (c) (not (member c (remove c xs))))])
    (andmap exists-once? xs)))

(: find-marker (->* (Input-Port Integer) ((Listof (U Char EOF)) Real) (U #f Real)))
(define (find-marker datastream marker-length [window '()] [char-count 1])
  (let ([next-window (cons (read-char datastream) window)])
    (cond
      [(eof-object? (car next-window)) #f]
      [(and (>= (length window) marker-length)
            (all-different? (take next-window marker-length)))
       char-count]
      [else 
        (find-marker datastream marker-length next-window (+ 1 char-count))])))

(: q6 (-> Integer String Real))
(define (q6 marker-length input)
  (or (find-marker (open-input-string input) marker-length)
      (error "Could not find marker")))

(display-advent-of-code-for-day 2022 6 (curry q6 4) (curry q6 14))

(provide all-different? find-marker)
