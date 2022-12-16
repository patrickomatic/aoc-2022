DAYS := $(patsubst %.rkt,%,$(wildcard day[1-9].rkt)) $(patsubst %.rkt,%,$(wildcard day[1-9][0-9].rkt))

all: $(DAYS)

%: %.rkt %-test.rkt shared.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

test: test.rkt
	raco exe test.rkt && ./test

.PHONY: clean
clean: 
	rm -rf compiled/ $(DAYS)
