#lang typed/racket

(define-type Coord Integer)
(define-type Point (Pairof Coord Coord))

(: clip-coord (->* (Coord) (#:clip-at Coord) Coord))
(define (clip-coord c #:clip-at [clip-at 0])
  (max clip-at c))

(: clip-point (->* (Point) (#:clip-at-x Coord #:clip-at-y Coord) Point))
(define (clip-point p 
                    #:clip-at-x [clip-at-x 0] 
                    #:clip-at-y [clip-at-y 0])
  (cons (clip-coord (car p) #:clip-at clip-at-x) 
        (clip-coord (cdr p) #:clip-at clip-at-y)))

(provide clip-coord clip-point)
