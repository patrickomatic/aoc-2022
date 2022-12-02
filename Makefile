all: day2

day2:
	racket day2.rkt

day1:
	racket day1.rkt

.PHONY: test
test: 
	raco test *-test.rkt
