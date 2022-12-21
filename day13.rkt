#lang racket
(require json)
(require advent-of-code)
(require "shared/number.rkt")

(define (load-packets input)
  (map (λ (l) (cons (string->jsexpr (first l)) (string->jsexpr (second l))))
       (map string-split (string-split input "\n\n"))))

(define (zip ax bx)
  (let* ([ax-list (if (number? ax) (list ax) ax)]
         [bx-list (if (number? bx) (list bx) bx)]
         [longest (max (length ax-list) (length bx-list))]
         [pad-list (λ (xs) (append xs (make-list (- longest (length xs)) #f)))])
    (for/list ([a (pad-list ax-list)] [b (pad-list bx-list)])
              (cons a b))))

(define (packets-ordered-p? a b return)
  (andmap (λ (p)
             (match p
                    [(cons (? false?) b) (return #t)]
                    [(cons a (? false?)) (return #f)]
                    [(cons (? number? a) (? number? b)) #:when (< a b) (return #t)]
                    [(cons (? number? a) (? number? b)) (= a b)]
                    [(cons a b) (packets-ordered-p? a b return)]))
          (zip a b)))

(define (packets-ordered? a b)
  (let/ec return (packets-ordered-p? a b return)))

(define (q12-part1 packets)
  (apply + (map add1 (indexes-where packets (λ (p) (packets-ordered? (car p) (cdr p)))))))

(define (q12-part2 packets)
  (let* ([unpaired-packets (append (map car packets) (map cdr packets) '(((2))) '(((6))))]
         [sorted (sort unpaired-packets packets-ordered?)])
    (* (add1 (index-of sorted '((2)))) (add1 (index-of sorted '((6)))))))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 13 #:cache #t)]
       [packets (load-packets input)])
  (printf "Question 13/Part 1: ~s\n" (q12-part1 packets))
  (printf "Question 13/Part 2: ~s\n" (q12-part2 packets)))

(provide packets-ordered?)
