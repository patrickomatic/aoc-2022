#lang racket
(require "shared/aoc.rkt" 
         "shared/number.rkt"
         "shared/grid.rkt")

(struct rope (knots tail-path)
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc r o o-mode)
           (let* ([knots (rope-knots r)]
                  [grid-size (max 6 (add1 (apply max (flatten knots))))]
                  [grid (make-grid grid-size ".")])
             (display-grid grid o o-mode (λ (p v)
                                            (let ([knot-pos (index-of knots p)])
                                              (cond
                                                [(equal? knot-pos 0) "H"]
                                                [(not (eq? #f knot-pos)) (number->string knot-pos)]
                                                [else "."]))))))])

(define s0 '(0 . 0))
(define short-rope (rope `(,s0 ,s0) `(,s0)))

(define l0 '(20 . 20))
(define long-rope (rope `(,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0 ,l0) `(,l0)))

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

(define (run-motions motions r)
  (if (null? motions)
    r
    (let* ([motion (car motions)]
           [direction (car motion)]
           [amount (cdr motion)])
      (run-motions (cdr motions) (repeat-motion r direction amount)))))
 
(define (q9 r input)
  (length
    (remove-duplicates
      (rope-tail-path
        (run-motions (load-motions input) r)))))

(display-advent-of-code-for-day 2022 9 (curry q9 short-rope) (curry q9 long-rope))

(provide move-knots rope run-motions points-adjacent? short-rope long-rope)
