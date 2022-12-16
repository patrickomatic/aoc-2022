#lang racket
(require json)
(require advent-of-code)
(require "shared.rkt")

(struct packet (a b))

(define (load-packets input)
  (map (λ (split) 
          (packet (string->jsexpr (first split)) (string->jsexpr (second split))))
       (map string-split (string-split input "\n\n"))))

(define (zip ax bx)
  (let* ([ax-list (if (number? ax) (list ax) ax)]
         [bx-list (if (number? bx) (list bx) bx)]
         [longest (max (length ax-list) (length bx-list))]
         [pad-list (λ (xs) (append xs (make-list (- longest (length xs)) #f)))])
    (for/list ([a (pad-list ax-list)] [b (pad-list bx-list)])
              (cons a b))))

(define (packets-ordered-p? p return)
  (andmap (λ (ab)
             (match ab
                    [(cons (? false?) b) (return #t)]
                    [(cons a (? false?)) (return #f)]
                    [(cons (? number? a) (? number? b)) #:when (< a b) (return #t)]
                    [(cons (? number? a) (? number? b)) (= a b)]
                    [(cons a b) (packets-ordered-p? (packet a b) return)]))
          (zip (packet-a p) (packet-b p))))

(define (packets-ordered? p)
  (call/cc (λ (return) (packets-ordered-p? p return))))

(define (q12 packets)
  (apply + (map add1 (indexes-where packets packets-ordered?))))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 13 #:cache #t)]
       [packets (load-packets input)])
  (printf "Question 13/Part 1: ~s\n" (q12 packets)))

(provide packet packets-ordered?)
