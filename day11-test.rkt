#lang racket

(require rackunit "day11.rkt")

(define test-monkeys (list (monkey 0 '(79 98) "old * 19" 23 2 3 0)
                           (monkey 1 '(54 65 75 74) "old + 6" 19 2 0 0)
                           (monkey 2 '(79 60 97) "old * old" 13 1 3 0)
                           (monkey 3 '(74) "old + 3" 17 0 1 0)))

(test-case "inspect-and-throw-items"
           (check-equal? (inspect-and-throw-items (car test-monkeys) test-monkeys relieved)
                         (list (monkey 0 '() "old * 19" 23 2 3 2)
                               (monkey 1 '(54 65 75 74) "old + 6" 19 2 0 0)
                               (monkey 2 '(79 60 97) "old * old" 13 1 3 0)
                               (monkey 3 '(74 500 620) "old + 3" 17 0 1 0))))

(test-case "monkey-test?"
           (check-equal? (monkey-test? (car test-monkeys) 23) #t)
           (check-equal? (monkey-test? (car test-monkeys) 46) #t)
           (check-equal? (monkey-test? (car test-monkeys) 24) #f)
           (check-equal? (monkey-test? (car test-monkeys) 21) #f))

(test-case "throw-to-monkey"
           (check-equal? (throw-to-monkey 98 0 1 test-monkeys)
                         (list (monkey 0 '(98) "old * 19" 23 2 3 1)
                               (monkey 1 '(54 65 75 74 98) "old + 6" 19 2 0 0)
                               (monkey 2 '(79 60 97) "old * old" 13 1 3 0)
                               (monkey 3 '(74) "old + 3" 17 0 1 0)))
           (check-equal? (throw-to-monkey 74 3 0 test-monkeys)
                         (list (monkey 0 '(79 98 74) "old * 19" 23 2 3 0)
                               (monkey 1 '(54 65 75 74) "old + 6" 19 2 0 0)
                               (monkey 2 '(79 60 97) "old * old" 13 1 3 0)
                               (monkey 3 '() "old + 3" 17 0 1 1))))

(test-case "take-turn"
           (check-equal? (take-turns test-monkeys 20 relieved)
                         (list
                            (monkey 0 '(10 12 14 26 34) "old * 19" 23 2 3 101)
                            (monkey 1 '(245 93 53 199 115) "old + 6" 19 2 0 95)
                            (monkey 2 '() "old * old" 13 1 3 7)
                            (monkey 3 '() "old + 3" 17 0 1 105))))
