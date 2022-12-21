#lang racket
(require advent-of-code)
(require "shared/sparse-matrix.rkt" 
         "shared/number.rkt" 
         "shared/grid.rkt" 
         "shared/geometry.rkt")

(struct sensor (location closest-beacon) #:transparent)

(define (load-readings input)
  (let ([p (λ (car-fn cdr-fn m) `(,(string->real (car-fn m)) . ,(string->real (cdr-fn m))))])
    (map (λ (m) (sensor (p first second m) (p third fourth m)))
         (map cdr
              (filter identity
                      (map (curry regexp-match #px"^Sensor\\s+at\\s+x=(\\d+),\\s+y=(\\d+):\\s+closest\\s+beacon\\s+is\\s+at\\s+x=(\\d+),\\s+y=(\\d+)$")
                        (string-split input "\n")))))))

(define (distance a b)
  (+ (abs (- (car a) (car b))) (abs (- (cdr a) (cdr b)))))

#|
(define empty-glyph ".")
(define sensor-glyph "S")
(define beacon-glyph "B")
(define not-occupied-glyph "#")

(define (display-sparse-matrix output-port sm origin width height sensors)
  (display-grid (sparse-matrix->grid sm origin width height)
               output-port
               #t
               (λ (p v) 
                  (if v not-occupied-glyph empty-glyph))))
|#

; XXX this doesn't work
(define (is-within-range? row-y sensor)
  #t)
#|
  (let* ([l (sensor-location sensor)]
         [d (distance l (sensor-closest-beacon sensor))])
    (or (row-y . < . (+ (cdr l) d)) 
        (row-y . > . (- (cdr l) d)))))
|#

(define (fill-within-range! sm s target-row)
  (printf "*** Filling in readings for sensor: ~s\n" s)
  (let* ([center (sensor-location s)]
         [unique-points (mutable-set)]
         [d (distance center (sensor-closest-beacon s))]
         ;[origin (clip-point `(,(- (car center) d) . ,(- (cdr center) d)))]
         ;[origin `(,(- (car center) d) . ,(- (cdr center) d))]
         ;[row (map (λ (x) ((distance center `(,(+ (car origin) x) . ,target-row)) . < . d)) 
         [row (filter (λ (x) ((distance center `(,(+ (car center) x) . ,target-row)) . < . d)) 
                      ;(range (* 2 d)))])
                      (inclusive-range (* -1 d) d))])
    (when ((length row) . > . 0)
      (printf "row=~s ~s\n" row (car (sort row <))))
    (set-union! sm (apply mutable-set row)))

    ;(sparse-matrix-add! sm target-row row))
  (printf "*** Finished filling in readings for sensor: ~s\n" s)
  sm)

(define (fill-in-sensors! sensors target-row)
  (let* ([sm (mutable-set)])
         (map (λ (s) (fill-within-range! sm s target-row)) sensors)
#|
         [threads (map (λ (s) (thread 
                                 (thunk 
                                   (fill-within-range! sm s target-row)))) 
                       sensors)])
    (for ([thread (in-list threads)])
         (thread-wait thread))
    |#
    sm))

(define (q15-part1 target-row sensors)
  (let* ([sm (fill-in-sensors! 
               (filter (curry is-within-range? target-row) sensors)
               target-row)])
    ; (display (sparse-matrix-row-ref sm target-row))
    ;(sparse-matrix-row-count sm target-row)))
    (set-count sm)))

#|
(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 15 #:cache #t)]
       [sensors (load-readings input)])
  (printf "Question 15/Part 1: ~s\n" (q15-part1 2000000 sensors)))
|#

(provide q15-part1
  ;display-sparse-matrix 
  fill-in-sensors! sensor)
