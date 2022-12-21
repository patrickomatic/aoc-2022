#lang racket
(require rackunit "day16.rkt")

(test-case "q16-part1"
           (check-equal? (most-pressure-relievable
                           `#hash(("AA" . ,(make-valve "AA" 0 '("DD" "II" "BB"))) 
                                  ("BB" . ,(make-valve "BB" 13 '("CC" "AA")))
                                  ("CC" . ,(make-valve "CC" 2 '("DD" "BB")))
                                  ("DD" . ,(make-valve "DD" 20 '("CC" "AA" "EE")))
                                  ("EE" . ,(make-valve "EE" 3 '("FF" "DD")))
                                  ("FF" . ,(make-valve "FF" 0 '("EE" "GG")))
                                  ("GG" . ,(make-valve "GG" 0 '("FF" "HH")))
                                  ("HH" . ,(make-valve "HH" 22 '("GG")))
                                  ("II" . ,(make-valve "II" 0 '("AA" "JJ")))
                                  ("JJ" . ,(make-valve "JJ" 21 '("II")))))
                         1651))
