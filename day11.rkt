#lang racket

(require advent-of-code)
(require "shared.rkt")

(struct monkey
        (id items operation test throw-to-if-true throw-to-if-false inspect-count)
        #:transparent)

(define (load-monkeys input)
  (map (λ (results) 
          (let ([matches (cdr results)])
            (monkey (string->real (first matches)) 
                    (map string->real (string-split (second matches) #px",\\s*"))
                    (third matches)
                    (string->real (fourth matches))
                    (string->real (fifth matches))
                    (string->real (sixth matches))
                    0)))
       (map (curry regexp-match #px"Monkey\\s+(\\d+):\\s+Starting\\s+items:\\s+([\\s\\d,]+)\n\\s+Operation:\\s+new\\s+=\\s+(.+?)\n\\s+Test:\\s+divisible\\s+by\\s+(\\d+)\n\\s+If\\s+true:\\s+throw\\s+to\\s+monkey\\s+(\\d+)\n\\s+If\\s+false:\\s+throw\\s+to\\s+monkey\\s+(\\d+)")
            (string-split input "\n\n"))))

(define (inspect-item m item)
  (let ([operation-match 
          (cdr (regexp-match #px"^old\\s(\\+|\\*)\\s+(old|\\d+)$" 
                             (monkey-operation m)))])
    (match operation-match
           [(list "+" "old") (+ item item)]
           [(list "*" "old") (* item item)]
           [(list "+" d) (+ item (string->real d))]
           [(list "*" d) (* item (string->real d))]
           [_ (error "Invalid operation" m)])))

(define (monkey-test? m item)
  (zero? (modulo item (monkey-test m))))

(define (relieved item)
  (floor (/ item 3)))

(define (throw-to-monkey item from-monkey-id to-monkey-id monkeys)
  (let* ([from (list-ref monkeys from-monkey-id)]
         [from-items (monkey-items from)]
         [to (list-ref monkeys to-monkey-id)]
         [to-items (monkey-items to)]
         [updated-monkeys (list-set 
                            (list-set monkeys from-monkey-id
                                      (struct-copy monkey from
                                                   [items (cdr from-items)]
                                                   [inspect-count (add1 (monkey-inspect-count from))]))
                            to-monkey-id
                            (struct-copy monkey to
                                         [items (append to-items (list item))]))])
         updated-monkeys))

(define (inspect-and-throw-items m monkeys)
  (foldl (λ (item updated-monkeys)
            (let* ([inspected-item (relieved (inspect-item m item))])
              (throw-to-monkey inspected-item
                               (monkey-id m)
                               (if (monkey-test? m inspected-item)
                                 (monkey-throw-to-if-true m)
                                 (monkey-throw-to-if-false m))
                               updated-monkeys)))
         monkeys
         (monkey-items m)))

(define (take-each-turn monkeys [monkey-id 0])
  (if (= monkey-id (length monkeys))
    monkeys
    (take-each-turn 
      (inspect-and-throw-items (list-ref monkeys monkey-id) monkeys)
      (add1 monkey-id))))

(define (take-turns monkeys [rounds 20])
  (if (zero? rounds)
    monkeys
    (take-turns (take-each-turn monkeys) (sub1 rounds))))

(define (q11 monkeys)
  (apply * 
         (map monkey-inspect-count 
              (take 
                (sort (take-turns monkeys) > #:key monkey-inspect-count) 
                2))))

(let* ([aoc-session (find-session)]
       [input (fetch-aoc-input aoc-session 2022 11 #:cache #t)]
       [monkeys (load-monkeys input)])
  (printf "Question 11/Part 1: ~s\n" (q11 monkeys)))

(provide inspect-and-throw-items load-monkeys monkey monkey-test? take-turns throw-to-monkey)
