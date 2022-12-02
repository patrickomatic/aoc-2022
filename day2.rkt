#lang typed/racket

(define-type Choice (U 'Rock 'Paper 'Scissors))

(struct strategy ([opponent : Choice] [me : Choice]))

(: i-win? (-> strategy Boolean))
(define (i-win? strat)
  (is-winner? (strategy-me strat) (strategy-opponent strat)))

(: they-win? (-> strategy Boolean))
(define (they-win? strat)
  (is-winner? (strategy-opponent strat) (strategy-me strat)))

(: is-winner? (-> Choice Choice Boolean))
(define (is-winner? a b)
  (or 
    (and (eq? a 'Rock) (eq? b 'Scissors))
    (and (eq? a 'Paper) (eq? b 'Rock))
    (and (eq? a 'Scissors) (eq? b 'Paper))))

(: string->choice (-> String Choice))
(define (string->choice str)
  (match (string->symbol str)
         ((or 'A 'X) 'Rock)
         ((or 'B 'Y) 'Paper)
         ((or 'C 'Z) 'Scissors)
         (else (error "Invalid input" str))))

(: score-for-win (-> strategy Integer))
(define (score-for-win strat)
  (cond
    ((i-win? strat) 6)
    ((they-win? strat) 0)
    (else 3)))

(: score-for-choice (-> strategy Integer))
(define (score-for-choice strategy)
  (match (strategy-me strategy)
    ['Rock 1]
    ['Paper 2]
    ['Scissors 3]))

(: load-strategies (-> String (Listof strategy)))
(define (load-strategies filename)
  (map (λ ([s : (Listof String)])
          (strategy (string->choice (first s)) (string->choice (second s))))
       (map string-split 
            (file->lines filename))))

(: q2-part1 (-> (Listof strategy) Real))
(define (q2-part1 strategies)
  (apply
    +
    (map
      (λ ([strat : strategy]) 
        (+ (score-for-win strat) (score-for-choice strat)))
      strategies)))

(let ([strategies (load-strategies "input/day2.txt" )])
  (q2-part1 strategies))
