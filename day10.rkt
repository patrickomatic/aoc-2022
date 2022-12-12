#lang racket

(require advent-of-code)
(require "shared.rkt")

(define (load-instructions input)
  (map (λ (line)
          (if (equal? line "noop")
            (cons 'noop '())
            (cons 'addx (string->real (first (cdr (string-split line)))))))
       (string-split input "\n")))

(define (fill-pipeline instructions)
  (flatten
    (map (λ (instruction)
            (if (eq? (car instruction) 'noop)
              instruction
              (list '(noop) (cdr instruction))))
         instructions)))

(define (sprite-overlaps-row-index? sprite-x row-index)
  (ormap (curry = row-index)
         (list (sub1 sprite-x) sprite-x (add1 sprite-x))))

(define (light-pixel crt x y)
  (vector-set! (vector-ref crt y) x "#"))

(define (run-cpu! instructions sample-cycles)
  (let* ([register-x 1]
         [pipeline (fill-pipeline instructions)]
         [current-instruction (car pipeline)]
         [samples '()]
         [row-index 0])
    (for ([cycle (range 1 (add1 (length pipeline)))])
         (set! current-instruction (car pipeline))
         (when (= row-index 0)
           (display "\n"))

         (when (member cycle sample-cycles)
           (set! samples (cons (* cycle register-x) samples)))

         (display (if (sprite-overlaps-row-index? register-x row-index) "#" "."))

         (when (not (eq? current-instruction 'noop))
           (set! register-x (+ register-x current-instruction)))

         (set! pipeline (cdr pipeline))
         (set! row-index (modulo (add1 row-index) 40)))
    (display "\n")
    samples))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 10 #:cache #t)]
       [instructions (load-instructions input)])
  (printf "Question 10/ Part 1: ~s\n" 
          (apply + (run-cpu! instructions '(20 60 100 140 180 220)))))

(provide fill-pipeline run-cpu! sprite-overlaps-row-index?)
