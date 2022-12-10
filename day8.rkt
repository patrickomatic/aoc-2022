#lang racket
(require advent-of-code)
(require "shared.rkt")

(define (load-grid input)
  ((compose
     (curry apply vector-immutable)
     (curry map (curry apply vector-immutable))
     (curry map (curry map (compose string->real string)))
     (curry map string->list)
     string-split)
   input))

(define (tree-height grid x y)
  (vector-ref (vector-ref grid y) x))

(define (grid-width grid) (vector-length (vector-ref grid 0)))

(define (grid-height grid) (vector-length grid))

(define (look-out-boolean grid x y height 
                          #:finished? finished?
                          #:fx [fx identity]
                          #:fy [fy identity])
  (cond 
    [(finished? x y) #f]
    [(>= (tree-height grid (fx x) (fy y)) height)]
    [else (look-out-boolean grid (fx x) (fy y) height
                            #:finished? finished? #:fx fx #:fy fy)]))

(define (look-out-count grid x y height [count 0]
                        #:finished? finished?
                        #:fx [fx identity]
                        #:fy [fy identity])
  (cond 
    [(finished? x y) count]
    [(>= (tree-height grid (fx x) (fy y)) height) (++ count)]
    [else (look-out-count grid (fx x) (fy y) height (++ count)
                          #:finished? finished? #:fx fx #:fy fy)]))

(define (viewing-distance grid x y direction look-out-fn)
  (let* ([height (tree-height grid x y)]
         [look (curry look-out-fn grid x y height)]
         [x= (λ (c) (λ (a b) (= a c)))]
         [y= (λ (c) (λ (a b) (= b c)))])
    (match direction
           ['up (look #:fy -- #:finished? (y= 0))]
           ['down (look #:fy ++ #:finished? (y= (-- (grid-height grid))))]
           ['left (look #:fx -- #:finished? (x= 0))]
           ['right (look #:fx ++ #:finished? (x= (-- (grid-width grid))))])))

(define (tree-is-visible? grid x y)
  (ormap (λ (direction) (not (viewing-distance grid x y direction look-out-boolean)))
         '(up down left right)))

(define (scenic-score grid x y)
  (apply * 
         (map (λ (direction) (viewing-distance grid x y direction look-out-count))
              '(up down left right))))

(define (get-visible-trees grid)
  (let ([visible-trees '()])
    (for/list ([y (grid-height grid)])
              (for/list ([x (grid-width grid)])
                        (and (tree-is-visible? grid x y)
                             (set! visible-trees (cons (tree-height grid x y) visible-trees)))))
    visible-trees))

(define (get-scenic-scores grid)
  (let ([scenic-scores '()])
    (for/list ([y (grid-height grid)])
              (for/list ([x (grid-width grid)])
                        (set! scenic-scores (cons (scenic-score grid x y) scenic-scores))))
    scenic-scores))

(define (q8 grid part)
  (if (eq? part 'part1)
    (length (get-visible-trees grid))
    (apply max (get-scenic-scores grid))))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 8 #:cache #t)]
       [grid (load-grid input)])
  (printf "Question 8/Part 1: ~s\n" (q8 grid 'part1))
  (printf "Question 8/Part 2: ~s\n" (q8 grid 'part2)))

(provide get-visible-trees tree-height)
