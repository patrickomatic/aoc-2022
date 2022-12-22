#lang typed/racket
(require "shared/aoc.rkt")

(define-type Items (Listof String))

(: split-item (-> String (Pairof String String)))
(define (split-item str)
  (let* ([len (string-length str)]
         [median (floor (/ len 2))])
    `(,(substring str 0 median) . ,(substring str median))))

(: priority (-> Char Integer))
(define (priority char)
  (- (char->integer char) 
     (if (char<? char #\a) 38 96)))

(: string->uniq-list (-> String (Listof Char)))
(define (string->uniq-list str)
  (remove-duplicates (string->list str)))

(: overlapping-chars (-> String (Listof String) (Listof Char)))
(define (overlapping-chars a bx)
  (let* ([uniq-a (string->uniq-list a)]
         [uniq-bx (map string->uniq-list bx)])
    (filter (λ ([c : Char])
               (andmap (λ ([l : (Listof Char)]) (member c l)) uniq-bx))
            uniq-a)))

(: item-overlap-score (-> String Integer))
(define (item-overlap-score item)
  (let ([s (split-item item)])
    (apply + 
           (map priority 
                (overlapping-chars (car s) (list (cdr s)))))))

(: group-items (-> Items (Listof Items)))
(define (group-items items)
   (if (not (empty? items))
     (cons
       (take items 3)
       (group-items (drop items 3)))
     '()))

(: group-overlap-score (-> Items Integer))
(define (group-overlap-score group)
  (apply +
         (map priority 
              (overlapping-chars (car group) (cdr group)))))

(: q3-part1 (-> String Integer))
(define (q3-part1 input)
  (apply + (map item-overlap-score (string-split input "\n"))))

(: q3-part2 (-> String Integer))
(define (q3-part2 input)
  (apply + (map group-overlap-score (group-items (string-split input "\n")))))

(display-advent-of-code-for-day 2022 3 q3-part1 q3-part2)

(provide item-overlap-score
         group-overlap-score
         group-items
         overlapping-chars
         priority
         split-item)
