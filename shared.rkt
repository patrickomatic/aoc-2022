#lang typed/racket

(: string->real (-> String Real))
(define (string->real s)
  (let ([p (string->number s)])
    (if p (real-part p) (error "Invalid number in input" s))))

(define-type Grid-Coord Nonnegative-Integer)
(define-type Grid-Point (Pairof Grid-Coord Grid-Coord))

(define-type (Gridof A) (Mutable-Vectorof (Mutable-Vectorof A)))

(: make-grid (All (A) (-> Nonnegative-Integer A (Gridof A))))
(define (make-grid size v)
  (make-vector size (make-vector size v)))

(: grid-width (All (A) (-> (Gridof A) Nonnegative-Integer)))
(define (grid-width grid)
  (vector-length (vector-ref grid 0)))

(: grid-height (All (A) (-> (Gridof A) Nonnegative-Integer)))
(define (grid-height grid)
  (vector-length grid))

(: grid-ref (All (A) (-> (Gridof A) Grid-Point A)))
(define (grid-ref grid p)
  (vector-ref (vector-ref grid (cdr p)) (car p)))

(: display-grid (All (A) (->* 
                           ((Gridof A) Output-Port (U Boolean 0 1))
                           ((U False (-> Grid-Point A String))) 
                           Void)))
(define (display-grid grid output output-mode [render-fn #f])
  (display "\n" output)
  (for ([y (grid-height grid)])
       (for ([x (grid-width grid)])
            (let* ([p (cons x y)]
                   [v (grid-ref grid p)])
              (display (if render-fn (render-fn p v) v) output)))
       (display "\n" output)))

(: grid-set! (All (A) (-> (Gridof A) Grid-Point A Void)))
(define (grid-set! grid p v)
  (vector-set! (vector-ref grid (cdr p)) (car p) v))

(provide Gridof
         Grid-Point 
         display-grid 
         grid-height
         grid-width
         grid-ref
         grid-set!
         make-grid
         string->real)
