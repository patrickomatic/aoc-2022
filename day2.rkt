#lang typed/racket

(define-type Choice (U 'Rock 'Paper 'Scissors))
(define-type Outcome (U 'Win 'Lose 'Draw))

(struct strategy
        ([them : Choice]
         [me : Choice]
         [desired-outcome : Outcome]))

(: winning-combos (Listof (Pairof Choice Choice)))
(define winning-combos 
  (list 
    (cons 'Rock 'Scissors)
    (cons 'Paper 'Rock)
    (cons 'Scissors 'Paper)))

(: losing-combos (Listof (Pairof Choice Choice)))
(define losing-combos 
  (map
    (λ ([combo : (Pairof Choice Choice)])
       (cons (cdr combo) (car combo)))
    winning-combos))

(: winner? (-> Choice Choice Boolean))
(define (winner? a b)
  (and (member (cons a b) winning-combos) #t))

(: string->outcome (-> String Outcome))
(define (string->outcome str)
  (match (string->symbol str)
         ('X 'Lose)
         ('Y 'Draw)
         ('Z 'Win)
         (else (error "Invalid outcome" str))))

(: string->choice (-> String Choice))
(define (string->choice str)
  (match (string->symbol str)
         ((or 'A 'X) 'Rock)
         ((or 'B 'Y) 'Paper)
         ((or 'C 'Z) 'Scissors)
         (else (error "Invalid choice" str))))

(: score-for-win (-> Choice Choice Integer))
(define (score-for-win me them)
  (cond
    ((winner? me them) 6)
    ((winner? them me) 0)
    (else 3)))

(: score-for-choice (-> Choice Integer))
(define (score-for-choice me)
  (match me
    ['Rock 1]
    ['Paper 2]
    ['Scissors 3]))

(: load-strategies (-> String (Listof strategy)))
(define (load-strategies filename)
  (map (λ ([s : (Listof String)])
          (strategy 
            (string->choice (first s))
            (string->choice (second s))
            (string->outcome (second s))))
       (map string-split 
            (file->lines filename))))

(: needed-to-win (-> Choice Choice))
(define (needed-to-win them)
  (cdr
    (or
      (assoc them losing-combos)
      (error "Cannot find combo" them))))

(: needed-to-lose (-> Choice Choice))
(define (needed-to-lose them)
  (cdr
    (or
      (assoc them winning-combos)
      (error "Cannot find combo" them))))

(: needed-to-draw (-> Choice Choice))
(define (needed-to-draw c)
  c)

(: choice-for-desired-outcome (-> strategy Choice))
(define (choice-for-desired-outcome strat)
  (let ([them (strategy-them strat)])
    (match (strategy-desired-outcome strat)
           ('Draw (needed-to-draw them))
           ('Win (needed-to-win them))
           ('Lose (needed-to-lose them)))))

(: q2-part1 (-> (Listof strategy) Real))
(define (q2-part1 strategies)
  (apply +
         (map
           (λ ([strat : strategy])
            (let ([them (strategy-them strat)]
                  [me (strategy-me strat)])
              (+ (score-for-win me them) (score-for-choice me))))
         strategies)))
  
(: q2-part2 (-> (Listof strategy) Real))
(define (q2-part2 strategies)
  (apply +
         (map
           (λ ([strat : strategy])
            (let ([them (strategy-them strat)]
                  [me (choice-for-desired-outcome strat)])
              (+ (score-for-win me them) (score-for-choice me))))
         strategies)))

(let ([strategies (load-strategies "input/day2.txt" )])
  (printf "Question 2/Part 1: ~a\n" (q2-part1 strategies))
  (printf "Question 2/Part 2: ~a\n" (q2-part2 strategies)))

(provide score-for-choice)
