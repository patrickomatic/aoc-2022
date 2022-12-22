#lang racket
(require "shared/aoc.rkt" 
         "shared/number.rkt" 
         "shared/grid.rkt")

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

;; TODO use the shared ones
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
    [(>= (tree-height grid (fx x) (fy y)) height) (add1 count)]
    [else (look-out-count grid (fx x) (fy y) height (add1 count)
                          #:finished? finished? #:fx fx #:fy fy)]))

(define (viewing-distance grid x y direction look-out-fn)
  (let* ([height (tree-height grid x y)]
         [look (curry look-out-fn grid x y height)]
         [x= (λ (c) (λ (a b) (= a c)))]
         [y= (λ (c) (λ (a b) (= b c)))])
    (match direction
           ['up (look #:fy sub1 #:finished? (y= 0))]
           ['down (look #:fy add1 #:finished? (y= (sub1 (grid-height grid))))]
           ['left (look #:fx sub1 #:finished? (x= 0))]
           ['right (look #:fx add1 #:finished? (x= (sub1 (grid-width grid))))])))

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

(define (q8 part input)
  (let ([grid (load-grid input)])
    (if (eq? part 'part1)
      (length (get-visible-trees grid))
      (apply max (get-scenic-scores grid)))))

(display-advent-of-code-for-day 2022 8 (curry q8 'part1) (curry q8 'part2))

(provide get-visible-trees tree-height)
