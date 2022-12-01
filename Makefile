all: test

.PHONY: test
test: 
	raco test *-test.rkt
