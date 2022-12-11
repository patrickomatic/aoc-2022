#lang racket

(require advent-of-code)
(require "shared.rkt")

(struct rope (knots tail-path)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc r o o-mode)
           (let* ([knots (rope-knots r)]
                  [grid-size (max 6 (add1 (apply max (flatten knots))))])
             (for ([y (range grid-size)])
                  (for ([x (range grid-size)])
                       (let* ([current-pos (cons x y)]
                              [knot-pos (index-of knots current-pos)])
                         (display (cond 
                                    [(equal? knot-pos 0) "H"]
                                    [(not (eq? #f knot-pos)) (number->string knot-pos)]
                                    [else "."]) 
                                  o)))
                  (fprintf o "\n"))))])

(define s0 '(0 . 0))
(define short-rope (rope `(,s0 ,s0) `(,s0)))

(define l0 '(20 . 20))
(define long-rope (rope `(,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0) `(,l0)))

; (: load-motions (-> String Motions))
(define (load-motions input)
  (map (λ (row)
          (let ([split-row (string-split row)])
            (cons (string->symbol (first split-row)) (string->real (second split-row)))))
       (string-split input "\n")))

(define (move-point p direction)
  (let ([x (car p)] [y (cdr p)])
    (match direction
           ['U (cons x (add1 y))]
           ['D (cons x (sub1 y))]
           ['L (cons (sub1 x) y)]
           ['R (cons (add1 x) y)]
           [else (error "Invalid move" direction)])))

(define (move-head direction r)
  (let ([knots (rope-knots r)])
    (struct-copy rope r [knots (cons (move-point (car knots) direction) (cdr knots))])))

(define (points-adjacent? a b)
  (let* ([xa (car a)] [ya (cdr a)]
         [xb (car b)] [yb (cdr b)]
         [x-distance (abs (- xa xb))]
         [y-distance (abs (- ya yb))])
    (and (<= x-distance 1) (<= y-distance 1)))) 

(define (move-knots r)
  (let* ([knots (rope-knots r)]
         [moved-knots (foldl (λ (a agg) 
                                (let* ([b (last agg)]
                                       [move-towards-point (λ (fn) (+ (fn a) 
                                                                     (cond [(> (fn a) (fn b)) -1]
                                                                           [(< (fn a) (fn b)) 1]
                                                                           [else 0])))])
                                  (append agg
                                          (list
                                            (if (points-adjacent? a b) a
                                              (cons (move-towards-point car) (move-towards-point cdr)))))))
                             (list (car knots))
                             (cdr knots))])
    (struct-copy rope r
                 [knots moved-knots]
                 [tail-path (cons (last moved-knots) (rope-tail-path r))])))

(define (repeat-motion r direction amount)
  (if (= amount 0)
    r
    (repeat-motion (move-knots (move-head direction r))
                   direction
                   (sub1 amount))))

; (: run-motions (->* (Motions) (Coordinate Coordinate (Listof Coordinate)) rope))
(define (run-motions motions r)
  (if (null? motions)
    r
    (let* ([motion (car motions)]
           [direction (car motion)]
           [amount (cdr motion)])
      (run-motions (cdr motions) (repeat-motion r direction amount)))))
 
; (: q9 (-> String (Listof Motion) Real))
(define (q9 motions r)
  (length
    (remove-duplicates
      (rope-tail-path
        (run-motions motions r)))))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 9 #:cache #t)]
       [motions (load-motions input)])
  (printf "Question 9/Part 1: ~s\n" (q9 motions short-rope))
  (printf "Question 9/Part 2: ~s\n" (q9 motions long-rope)))

(provide move-knots rope run-motions points-adjacent? short-rope long-rope)
