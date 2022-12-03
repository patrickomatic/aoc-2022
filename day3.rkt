#lang typed/racket

(define-type Items (Listof String))

(: split-item (-> String (Pairof String String)))
(define (split-item str)
  (let* ([len (string-length str)]
         [median (floor (/ len 2))])
    (cons (substring str 0 median) (substring str median))))

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
               (andmap (λ ([l : (Listof Char)])
                          (member c l))
                       uniq-bx))
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

(: q3-part1 (-> Items Integer))
(define (q3-part1 items)
  (apply + (map item-overlap-score items)))

(: q3-part2 (-> Items Integer))
(define (q3-part2 items)
  (apply + (map group-overlap-score (group-items items))))

(let ([items (file->lines "input/day3.txt" )])
  (printf "Question 3/Part 1: ~a\n" (q3-part1 items))
  (printf "Question 3/Part 2: ~a\n" (q3-part2 items)))

(provide item-overlap-score
         group-overlap-score
         group-items
         overlapping-chars
         priority
         split-item)
