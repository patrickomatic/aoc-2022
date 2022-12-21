#lang racket
#|
(require data/bit-vector)
(require rackunit "sparse-matrix.rkt")

; TODO there's gotta be a better way than casting to a string
(define (pr r) (format "~s" r))

(test-case "make-sparse-matrix"
           (check-equal? (pr (make-sparse-matrix)) "#hash()"))

(test-case "sparse-matrix-add!"
           (check-equal? (pr (sparse-matrix-add! 
                           (make-sparse-matrix) 
                           5 
                           4
                           '(#t #t #t #f #f #f #t #t)))
                         "#hash((5 . (#(struct:row 4 #<bit-vector>))))"))

(define (test-sparse-matrix-with-rows) 
  (let ([sm (make-sparse-matrix)])
    (sparse-matrix-add! sm 5 4 '(#t #f #f #t))
    (sparse-matrix-add! sm 5 10 '(#t #t #t))
    sm))

(define (test-sparse-matrix-with-overlapping-rows) 
  (let ([sm (make-sparse-matrix)])
    (sparse-matrix-add! sm 5 4 '(#t #f #f #t))
    (sparse-matrix-add! sm 5 4 '(#t #t #t))
    sm))

(test-case "sparse-matrix-row-ref"
           (check-equal? (sparse-matrix-row-ref (test-sparse-matrix-with-rows) 5)
                         '(#f #f #f #f #t #f #f #t #f #f #t #t #t))
           (check-equal? (sparse-matrix-row-ref (test-sparse-matrix-with-rows) 5)
                         '(#f #f #f #f #t #t #t #t)))

(test-case "sparse-matrix->grid"
           (check-equal? (sparse-matrix->grid (test-sparse-matrix-with-rows) '(0 . 0) 7 13)
                         '#(
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #t #f #f #t #f #f #t #t #t)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f)
                            #(#f #f #f #f #f #f #f #f #f #f #f #f #f))))
|#
