#lang racket

(require "shared.rkt")

(struct command (amount from to))

(define command-regexp 
  (const #px"^move\\s+(\\d+)\\s+from\\s+(\\d+)\\s+to\\s+(\\d+)$"))

(define stack-regexp 
  (const #px"^(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))\\s+(?:\\[(\\w)\\]|(\\s\\s\\s))$"))

(define (parse-stacks lines) (let* ([stacks (make-vector 9 '())]
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

(define (move-crate! stacks from to)
  (let* ([from-index (stack-index from)]
         [from (vector-ref stacks from-index)]
         [to-index (stack-index to)]
         [to (vector-ref stacks to-index)])
    (or (empty? from)
        (begin
          (vector-set! stacks to-index (append to (list (last from))))
          (vector-set! stacks from-index (drop-right from 1))))))

(define (run-commands! stacks commands)
  (for/list ([command commands])
            (for/list ([i (range (command-amount command))])
                      (move-crate! stacks (command-from command) (command-to command)))))

(define (q1-part1 stacks commands)
  (begin
    (run-commands! stacks commands)
    (string-join (vector->list (vector-map last (vector-filter-not empty? stacks))) "")))

(let* ([filename "input/day5.txt"]
       [lines (file->lines filename)]
       [commands (parse-commands lines)]
       [stacks (parse-stacks lines)])
  (printf "Question 5/Part 1: ~s\n" (q1-part1 stacks commands)))

(provide command parse-commands)
