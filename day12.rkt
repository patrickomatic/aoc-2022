#lang racket

(require advent-of-code)

(define start-marker #\S)
(define end-marker #\E)
(define end-marker-elevation (add1 (char->integer #\z)))

(define (ec h p)
  (vector-ref (vector-ref (heightmap-elevations h) (cdr p)) (car p)))

(struct heightmap (start end elevations shortest-path) 
        #:transparent
        #:methods gen:custom-write
        [(define (write-proc h o o-mode)
           (let ([elevations (heightmap-elevations h)])
             (display "\n" o)
             (for ([y (vector-length elevations)])
                  (for ([x (vector-length (vector-ref elevations 0))])
                       (let ([p (cons x y)]
                             [shortest-path (heightmap-shortest-path h)])
                         (display 
                           (if (member p shortest-path) "." (ec h (cons x y)))
                           o)))
                  (display "\n" o))))])

(define (elevation h p)
  (let ([c (ec h p)])
    (if (eq? c end-marker)
      (add1 (char->integer #\z))
      (char->integer c))))

(define (find-marker-in-row m row y)
  (let ([x (vector-memq m row)])
    (if x (cons x y) #f)))

(define (find-marker elevations m [y 0])
  (if (eq? y (vector-length elevations))
    #f
    (or (find-marker-in-row m (vector-ref elevations y) y)
        (find-marker elevations m (add1 y)))))
 
(define (load-heightmap input)
  (let ([elevations (list->vector
                     (map (compose list->vector string->list)
                          (string-split input "\n")))])
    (heightmap 
      (find-marker elevations start-marker)
      (find-marker elevations end-marker)
      elevations
      '())))

(define (can-handle-elevation-change? h p p-to)
  (let ([p-char (ec h p)]
        [p-to-char (ec h p-to)]
        [e (elevation h p)]
        [e-to (elevation h p-to)])
    (or (eq? p-char start-marker)
        (>= e e-to)
        (= (- e-to e) 1))))

(define (within-bounds? h p)
  (let* ([elevations (heightmap-elevations h)]
         [x (car p)]
         [y (cdr p)]
         [grid-height (vector-length elevations)]
         [grid-width (vector-length (vector-ref elevations 0))])
    (and (>= x 0) (>= y 0)
         (< x grid-width) (< y grid-height))))

(define (get-adjacent-steps h p)
  (let ([x (car p)] [y (cdr p)])
    (filter (curry can-handle-elevation-change? h p)
            (filter (curry within-bounds? h)
                    `((,x . ,(add1 y))
                      (,(add1 x) . ,y)
                      (,x . ,(sub1 y))
                      (,(sub1 x) . ,y))))))

(define (bfs h start)
  (let ([queue (list (list start))]
        [p #f]
        [path '()]
        [visited (mutable-set start)]
        [discovered-path #f])
    (do ()
      [(or discovered-path (empty? queue))]
      (set! path (car queue))
      (set! p (car path))

      (if (eq? (ec h p) end-marker)
        (set! discovered-path path)
        (let ([to-search (remove* (set->list visited) 
                                  (get-adjacent-steps h p))])
          (for ([new-p to-search]) 
               (set-add! visited new-p))
          (set! queue (append (cdr queue)
                              (map (curryr cons path) to-search))))))
    (and discovered-path
         (struct-copy heightmap h
                      [shortest-path (reverse discovered-path)]))))

(define (q12-part1 h)
  (bfs h (heightmap-start h)))

(define (q12-part2 h)
  (let* ([elevations (heightmap-elevations h)]
         [all-low-points (filter identity (apply append
                                (for/list ([y (vector-length elevations)])
                                          (for/list ([x (vector-length (vector-ref elevations 0))])
                                                    (let ([p (cons x y)])
                                                      (if (eq? (ec h p) #\a) p #f))))))]
         [all-searches (filter identity (map (curry bfs h) all-low-points))])
    (first (sort (map heightmap-shortest-path all-searches) < #:key length))))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 12 #:cache #t)]
       [h (load-heightmap input)])
  (printf "## Question 12\n~s\n" h)

  (let ([from-start (q12-part1 h)])
    (display from-start)
    (printf "Question 12/Part 1: ~s\n" (- (length (heightmap-shortest-path from-start)) 3)))

  (let ([from-low-point (q12-part2 h)])
    (printf "Question 12/Part 2: ~s\n" (- (length from-low-point) 3))))

(provide bfs
         can-handle-elevation-change? 
         elevation 
         find-marker 
         get-adjacent-steps
         heightmap 
         heightmap-elevations
         heightmap-shortest-path
         within-bounds?)
