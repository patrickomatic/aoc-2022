#lang typed/racket

(: string->real (-> String Real))
(define (string->real s)
  (let ([p (string->number s)])
    (if p (real-part p) (error "Invalid number in input" s))))

(define-type Grid-Coord Nonnegative-Integer)
(define-type Grid (Vectorof (Vectorof Any)))
(define-type Grid-Point (Pairof Grid-Coord Grid-Coord))

(: grid-width (-> Grid Nonnegative-Integer))
(define (grid-width grid)
  (vector-length (vector-ref grid 0)))

(: grid-height (-> Grid Nonnegative-Integer))
(define (grid-height grid)
  (vector-length grid))

(: grid-ref (-> Grid Grid-Point Any))
(define (grid-ref grid p)
  (vector-ref (vector-ref grid (cdr p)) (car p)))

;; TODO use output-mode
(: display-grid (->* (Grid Output-Port (U Boolean 0 1)) ((U False (-> Grid-Point Any String))) Void))
(define (display-grid grid output output-mode [render-fn #f])
  (display "\n" output)
  (for ([y (grid-height grid)])
       (for ([x (grid-width grid)]) ;; XXX I think I might be able to group these two together
            (let* ([p (cons x y)]
                   [v (grid-ref grid p)])
              (display (if render-fn (render-fn p v) v) output)))
       (display "\n" output)))

(: grid-set! (-> Grid Grid-Point Any Void))
(define (grid-set! grid p v)
  (vector-set! (vector-ref grid (cdr p)) (car p) v))

;(: grid-set (-> Grid Point Any Grid))
;(define (grid-set grid p v)
  ;(vector-set (vector-ref grid (cdr p)) (car p)))

(provide Grid 
         Grid-Point 
         display-grid 
         grid-height
         grid-width
         grid-ref
         ;grid-set
         grid-set!
         string->real)
