DAYS := $(patsubst %.rkt,%,$(wildcard day[0-9].rkt))

all: $(DAYS)

%: %.rkt %-test.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

.PHONY: clean
clean: 
	rm -rf compiled/ $(DAYS)
