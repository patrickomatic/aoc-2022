#lang racket
; TODO add types
(require "grid.rkt"
         "geometry.rkt")

;(struct row (start-index coords) #:transparent)

(define (make-sparse-matrix) (make-hash))

(define (sparse-matrix-row-count sm y)
  (let ([row (hash-ref sm y (set))])
    (printf "first? ~s\n" (set-first row))
    (printf "last? ~s\n" (length (set->list (set-rest row))))
    (set-count row)))
               #|
    (foldl (λ (row-segment point-set)
              (set-union point-set 
                         (list->set (indexes-of (bit-vector->list (row-bv row-segment)) #t))))
           (set)
           (sort (hash-ref sm y '()) < #:key row-start-index))))
  |#


#|    
    (foldl (λ (row-segment b)
              (let* ([bytes (string->number (bit-vector->string (row-bv)) 2)])
                (bitwise-ior
                  (arithmetic-shift b (row-start-index row-segment))
                  bytes)))
           0
           row-segments)))
|#
#|
    (flatten
      (foldl (λ (row-segment row-result)
                (printf "appending ~s ~s\n" (row-start-index row-segment) (length row-result))
                (append 
                  row-result
                  (make-list (- (row-start-index row-segment) (length row-result)) #f)
                  (bit-vector->list (row-bv row-segment))))
             '()
             row-segments))))
|#

(define (sparse-matrix-add! sm y row-values)
  (let ([current-row (hash-ref sm y (set))])
        ;[bv (apply bit-vector row-values)])
    ; (set-union! current-row (apply set row-values))
    (hash-set! sm y (set-union current-row (apply set row-values)))
    sm))

#|
(define (sparse-matrix->grid sm origin width height)
  (build-vector 
    height 
    (λ (y) 
       (let ([row (sparse-matrix-row-ref sm y)])
         (list->vector (append row 
                               (make-list (clip-coord (- height (length row))) #f)))))))
|#

(provide make-sparse-matrix 
         ;sparse-matrix->grid
         sparse-matrix-add! 
         sparse-matrix-row-count)
