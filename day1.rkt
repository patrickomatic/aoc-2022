#lang typed/racket

(define-type Calories Real)

(define-type ParsedEntry (U Calories 'Separator))

(: group-calories (-> (Listof ParsedEntry) (Listof Calories)))
(define (group-calories [entries : (Listof ParsedEntry)])
  (foldr
   (λ ([v : ParsedEntry] [current : (Listof Calories)])
     (if (eq? v 'Separator)
         (cons 0 current)
         (cons (+ v (car current)) (cdr current))
         ))
   '(0)
   entries
   ))
   
(: parse-input (-> (Listof String) (Listof ParsedEntry)))
(define (parse-input lines)
  (map
   (λ ([s : String])
     (let ([parsed-number (string->number s 10)])
       (if (eq? #f parsed-number)
           'Separator
           (real-part parsed-number))))
   lines
  ))

(: q1-part1 (-> (Listof String) Calories))
(define (q1-part1 [lines : (Listof String)])
  (apply
   max
   (group-calories
    (parse-input lines))))

(: q1-part2 (-> (Listof String) Calories))
(define (q1-part2 [lines : (Listof String)])
   (apply
    +
    (take
     (sort
      (group-calories
       (parse-input lines))
      >)
     3)))

(q1-part1 (file->lines "input/day1.txt"))
(q1-part2 (file->lines "input/day1.txt"))

(provide group-calories
         parse-input)
