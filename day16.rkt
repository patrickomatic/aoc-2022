#lang racket
(require advent-of-code memo)
(require "shared/number.rkt")

(define (make-valve id flow-rate leads-to)
  (valve id flow-rate leads-to #f))

(struct valve (id flow-rate leads-to open?) #:transparent)
        
(define (load-valves input)
  ((compose
     make-hash
     (curry map (λ (m) 
                   (let ([valve-id (first m)]
                         [neighbor-valve-ids (string-split (third m) ", ")])
                     `(,valve-id . ,(make-valve valve-id (string->real (second m)) neighbor-valve-ids)))))
     (curry map cdr)
     (curry filter identity)
     (curry map 
            (curry regexp-match 
                   #px"^Valve (\\w+) has flow rate=(\\d+); tunnels lead to valves (.+)$")))
   (string-split input "\n")))

(define (open-valve valves id)
  (hash-set valves id 
            (struct-copy valve (hash-ref valves id) 
                         [open? #t])))

(define/memoize 
  (valve-search v minutes-left all-valves)
;(define (valve-search v minutes-left all-valves)
  (let* ([flow-rate (valve-flow-rate v)]
         [minutes-to-open-valve (if (zero? flow-rate) 0 1)]
         [flow-from-opening (* (sub1 minutes-left) flow-rate)]
         [neighbors (map (curry hash-ref all-valves) (valve-leads-to v))])
    ;(printf "current valve=~s minutes-left=~s cumulative-flow=~s\n" v minutes-left flow-from-opening)
    (cond 
      [(<= minutes-left 1) 0]
      #|
      [(member (valve-id v) open-valves) 
       ;; don't flip valve, just visit neighbors
       (apply max (map (λ (neighbor) 
                          (+ 0 (valve-search neighbor (sub1 minutes-left) all-valves)))
                       neighbors))]
      |#
      [else (apply max 
                   (append 
                     (list flow-from-opening)

                     ;; don't switch our valve and visit neighbors
                     (map (λ (neighbor) 
                             (+ 0 (valve-search neighbor (sub1 minutes-left) all-valves)))
                          neighbors)

                     ;; switch our valve and visit neighbor
                     (map (λ (neighbor) 
                             (+ flow-from-opening
                                (valve-search neighbor
                                              (sub1 (- minutes-left minutes-to-open-valve))
                                              all-valves)))
                                              ;(open-valve all-valves (valve-id v)))))
                          neighbors)))])))

(define (most-pressure-relievable valves)
  (valve-search (hash-ref valves "AA") 30 valves))

#|
(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 16 #:cache #t)]
       [valves (load-valves input)])
  (printf "Question 16/Part 1: ~s\n" (most-pressure-relievable valves)))
|#

(provide most-pressure-relievable make-valve)
