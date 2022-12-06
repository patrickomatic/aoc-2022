#lang typed/racket

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

(: q6 (-> String Integer Real))
(define (q6 input marker-length)
  (or (find-marker (open-input-file input) marker-length)
      (error "Could not find marker")))

(let* ([input "input/day6.txt"])
  (printf "Question 6/Part 1: ~s\n" (q6 input 4))
  (printf "Question 6/Part 2: ~s\n" (q6 input 14)))

(provide all-different? find-marker)
