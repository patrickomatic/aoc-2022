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

(: overlapping-chars (-> String String (Listof Char)))
(define (overlapping-chars a b)
  (let ([b-list (remove-duplicates (string->list b))])
    (filter (λ ([c : Char])
               (member c b-list))
            (remove-duplicates (string->list a)))))

(: overlap-score (-> String Integer))
(define (overlap-score item)
  (let ([s (split-item item)])
    (apply + 
           (map priority 
                (overlapping-chars (car s) (cdr s))))))

(: q3-part1 (-> Items Integer))
(define (q3-part1 items)
  (apply + (map overlap-score items)))

(: q3-part2 (-> Items Integer))
(define (q3-part2 items)
  0)

(let ([items (file->lines "input/day3.txt" )])
  (printf "Question 3/Part 1: ~a\n" (q3-part1 items))
  (printf "Question 3/Part 2: ~a\n" (q3-part2 items)))

(provide overlapping-chars
         overlap-score
         priority
         split-item)
