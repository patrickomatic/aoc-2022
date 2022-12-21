#lang racket
(require advent-of-code)
(require "shared/number.rkt" "shared/grid.rkt")

(define rock "▩")
(define sand ".")
(define darkness " ")

(struct viewport (origin width height) #:transparent)

(struct cave (grid scans sand-at-rest viewport drop-sand-from floor-y) 
        #:mutable #:transparent
        #:methods gen:custom-write
        [(define (write-proc c output output-mode)
           (let ([vp (cave-viewport c)])
             (display-grid 
               (sub-grid (cave-grid c)
                         (viewport-origin vp)
                         (viewport-width vp)
                         (viewport-height vp))
               output output-mode)))])

(define (make-cave scans 
                   #:vp vp 
                   #:drop-sand-from drop-sand-from 
                   #:floor-y [floor-y #f])
  (let ([grid-size (* 2 (apply max (flatten scans)))])
    (cave (make-grid grid-size darkness) scans '() vp drop-sand-from floor-y)))

(define (draw-line! cave char a b)
  (let* ([ax (car a)]
         [ay (cdr a)]
         [bx (car b)]
         [by (cdr b)]
         [distance (max (abs (- bx ax)) (abs (- by ay)))]
         [draw! (curry draw-point! cave char)])
    (for ([d (range (add1 distance))])
         (cond 
           [(ay . < . by) (draw! (cons ax (+ ay d)))]
           [(ay . > . by) (draw! (cons ax (- ay d)))]
           [(ax . > . bx) (draw! (cons (- ax d) ay))]
           [(ax . < . bx) (draw! (cons (+ ax d) ay))]))
    cave))

(define (draw-point! c char p)
  (grid-set! (cave-grid c) p char))

(define (draw-rocks! c)
  (for ([scan (cave-scans c)])
       (for/fold ([a (car scan)]) ([b (cdr scan)])
                 (draw-line! c rock a b)
                 (values b))))

(define (draw-floor! c)
  (let ([floor-y (cave-floor-y c)]
        [grid (cave-grid c)])
    (when floor-y
      (draw-line! c rock `(0 . ,floor-y) `(,(sub1 (grid-width grid)) . ,floor-y))))
  c)

(define (load-coordinate str)
  (match (string-split str ",")
         [(list a b) (cons (string->real a) (string->real b))]
         [_ (error "Invalid coordinate" str)]))

(define (load-scans input)
  (map (λ (line) (map load-coordinate (regexp-split #px"\\s+->\\s+" line)))
       (string-split input "\n")))

(define is-clear? (curry equal? darkness))

(define (drop-grain-of-sand! c)
  (let ([at-rest? #f]
        [into-the-abyss? #f]
        [backed-up? #f]
        [grid (cave-grid c)]
        [p (cave-drop-sand-from c)])
    (do () [(or at-rest? into-the-abyss? backed-up?)]
      (let* ([x (car p)] 
             [y (cdr p)]
             [v (λ (x y) (grid-ref grid (cons x (add1 y))))]
             [fall! (λ (shift-x) (set! p (cons (+ shift-x x) (add1 y))))])
        (match (list (v (sub1 x) y) (v x y) (v (add1 x) y))
               [(list _ a _) #:when (is-clear? a) (fall! 0)]
               [(list a _ _) #:when (is-clear? a) (fall! -1)] 
               [(list _ _ a) #:when (is-clear? a) (fall! 1)] 
               [_ (set! at-rest? #t)]) 

        (when (= (cdr p) 0)
          (draw-point! c sand p)
          (set! backed-up? #t))

        (when (= (cdr p) (sub1 (grid-height grid)))
          (set! into-the-abyss? #t))))

    (if (or into-the-abyss? backed-up?)
      #f 
      (begin
        (draw-point! c sand p)
        p))))

(define (turn-on-the-sand! c)
  (let ([sand-overflowing? #f]
        [sand '()])
    (do () [sand-overflowing?]
      (let ([resting-at (drop-grain-of-sand! c)])
        (if resting-at 
            (set! sand (cons resting-at sand))
            (set! sand-overflowing? #t))))

    (set-cave-sand-at-rest! c sand)))

(define (animate-sand c) 
  (begin
    (draw-rocks! c)
    (draw-floor! c)
    (turn-on-the-sand! c)
    (display c)
    c))

(define (sand-resting-after-animation cave)
  ((compose length cave-sand-at-rest animate-sand) cave))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 14 #:cache #t)]
       [scans (load-scans input)]
       [max-scan-y (apply max (flatten (map (λ (p) (map cdr p)) scans)))]
       [floor-y (+ 2 max-scan-y)])
  (printf "Question 14/Part 1: ~s\n" 
          (sand-resting-after-animation (make-cave scans 
                                                   #:vp (viewport '(450 . 0) 59 170)
                                                   #:drop-sand-from '(500 . 0))))
  (printf "Question 14/Part 2: ~s\n" 
          (add1 (sand-resting-after-animation (make-cave scans 
                                                         #:vp (viewport '(450 . 0) 59 172)
                                                         #:drop-sand-from '(500 . 0)
                                                         #:floor-y floor-y)))))

(provide animate-sand cave darkness draw-line! make-cave rock sand viewport)
