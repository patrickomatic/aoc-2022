DAYS := day1 day2 day3

all: $(DAYS)

%: %.rkt
	(test -f $@-test.rkt && raco test $@-test.rkt) || exit 0
	raco exe $@.rkt

.PHONY: clean
clean: 
	rm -rf compiled/ $(DAYS)
