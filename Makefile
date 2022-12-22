DAYS := $(patsubst %.rkt,%,$(wildcard day[1-9].rkt)) $(patsubst %.rkt,%,$(wildcard day[1-9][0-9].rkt))

NUMBER_LIB := shared/compiled/number_rkt.zo
GEOMETRY_LIB := shared/compiled/geometry_rkt.zo
GRID_LIB := shared/compiled/grid_rkt.zo
SPARSE_MATRIX_LIB := shared/compiled/sparse-matrix_rkt.zo
SHARED_LIBS := $(NUMBER_LIB) $(GRID_LIB) $(SPARSE_MATRIX_LIB)

RACKET_PACKAGE_DEPS := advent-of-code memo

all: $(DAYS)
#all: day16

shared/compiled/geometry_rkt.zo: shared/geometry.rkt shared/geometry-test.rkt
	raco test shared/geometry-test.rkt
	raco make shared/geometry.rkt

$(NUMBER_LIB): shared/number.rkt shared/number-test.rkt
	raco test shared/number-test.rkt
	raco make shared/number.rkt

shared/compiled/grid_rkt.zo: shared/grid.rkt shared/grid-test.rkt
	raco test shared/grid-test.rkt
	raco make shared/grid.rkt

$(SPARSE_MATRIX_LIB): shared/sparse-matrix.rkt shared/sparse-matrix-test.rkt
	raco test shared/sparse-matrix-test.rkt
	raco make shared/sparse-matrix.rkt

day1: day1.rkt day1-test.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

day2: day2.rkt day2-test.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

day3: day3.rkt day3-test.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

day4: day4.rkt day4-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day5: day5.rkt day5-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day6: day6.rkt day6-test.rkt
	raco test $@-test.rkt
	raco exe $@.rkt

day7: day7.rkt day7-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day8: day8.rkt day8-test.rkt $(NUMBER_LIB) $(GRID_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day9: day9.rkt day9-test.rkt $(NUMBER_LIB) $(GRID_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day10: day10.rkt day10-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day11: day11.rkt day11-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day12: day12.rkt day12-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day13: day13.rkt day13-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day14: day14.rkt day14-test.rkt $(NUMBER_LIB) $(GRID_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day15: day15.rkt day15-test.rkt $(NUMBER_LIB) $(GRID_LIB) $(SPARSE_MATRIX_LIB) $(GEOMETRY_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

day16: day16.rkt day16-test.rkt $(NUMBER_LIB)
	raco test $@-test.rkt
	raco exe $@.rkt

test: test.rkt
	raco exe test.rkt && ./test

.PHONY: dependencies
dependencies:
	raco pkg install --skip-installed $(RACKET_PACKAGE_DEPS)

.PHONY: clean
clean: 
	rm -rf compiled/ shared/compiled/ $(DAYS)
