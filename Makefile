DAYS := day1 day2 day3

all: $(DAYS)

%: %.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

.PHONY: clean
clean: 
	rm -rf compiled/ $(DAYS)
