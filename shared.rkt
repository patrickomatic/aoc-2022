#lang typed/racket

(: string->real (-> String Real))
(define (string->real s)
  (let ([p (string->number s)])
    (if p (real-part p) (error "Invalid number in input" s))))

(define-type Grid-Coord Nonnegative-Integer)
(define-type Grid-Point (Pairof Grid-Coord Grid-Coord))

(define-type (Immutable-Gridof A) (Vectorof (Vectorof A)))
(define-type (Gridof A) (Mutable-Vectorof (Mutable-Vectorof A)))

(: make-grid (All (A) (-> Nonnegative-Integer A (Gridof A))))
(define (make-grid size v)
  (build-vector size (λ (row) (make-vector size v))))

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
  (let ([row (vector-ref grid (cdr p))])
    (vector-set! row (car p) v)))

(: sub-grid (All (A) (-> (Gridof A) Grid-Point Integer Integer (Gridof A))))
(define (sub-grid grid origin width height)
  (let ([x0 (car origin)] [y0 (cdr origin)])
    (build-vector height (λ ([y : Nonnegative-Integer]) 
                            (build-vector width (λ ([x : Nonnegative-Integer]) 
                                                   (grid-ref grid (cons (+ x0 x) (+ y0 y)))))))))

(: grid->mutable-grid (All (A) (-> (Immutable-Gridof A) (Gridof A))))
(define (grid->mutable-grid grid)
  (build-vector (vector-length grid) 
                (λ ([y : Integer])
                   (vector-copy (vector-ref grid y)))))

(provide Gridof
         Grid-Point 
         display-grid 
         grid->mutable-grid
         grid-height
         grid-width
         grid-ref
         grid-set!
         make-grid
         string->real
         sub-grid)
