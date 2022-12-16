#lang racket
(require "shared.rkt")

(struct command (amount from to))

(define command-regexp 
  (const #px"^move\\s+(\\d+)\\s+from\\s+(\\d+)\\s+to\\s+(\\d+)$"))

(define stack-regexp 
  (const #px"^(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))$"))

(define (parse-stacks lines)
  (let ([stacks (make-vector 9 '())]
        [matched-lines (map (curry filter-map identity)
                            (map cdr
                                 (filter-map (curry regexp-match (stack-regexp)) lines)))])
    (for/list ([matched-line matched-lines])
              (for/list ([(e i) (in-indexed matched-line)])
                        (or (equal? e "   ")
                            (vector-set! stacks i (cons e (vector-ref stacks i))))))
    stacks))

(define (parse-commands lines)
  (map (λ (m) (apply command (map string->real m)))
       (map (curryr take-right 3)
            (filter-map (curry regexp-match (command-regexp)) lines))))

(define (stack-index i) (- i 1))

(define (move-crates! stacks from to amount)
  (let* ([from-index (stack-index from)]
         [from (vector-ref stacks from-index)]
         [to-index (stack-index to)]
         [to (vector-ref stacks to-index)])
    (or (empty? from)
        (begin
          (vector-set! stacks to-index (append to (take-right from amount)))
          (vector-set! stacks from-index (drop-right from (min (length from) amount)))))))

(define (run-commands! stacks commands strategy)
  (for/list ([command commands])
            (let ([amount (command-amount command)]
                  [to (command-to command)]
                  [from (command-from command)])
              (if (eq? strategy 'one-at-a-time)
                (for/list ([i (range amount)]) (move-crates! stacks from to 1))
                (move-crates! stacks from to amount)))))

(define (q5 stacks commands strategy)
  (begin
    (run-commands! stacks commands strategy)
    (string-join (vector->list (vector-map last (vector-filter-not empty? stacks))) "")))

(let* ([filename "input/day5.txt"]
       [lines (file->lines filename)]
       [commands (parse-commands lines)])
  (printf "Question 5/Part 1: ~s\n" (q5 (parse-stacks lines) commands 'one-at-a-time))
  (printf "Question 5/Part 2: ~s\n" (q5 (parse-stacks lines) commands 'multiple)))

(provide command parse-commands)
