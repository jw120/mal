.PHONY: all clean distclean run test step-test self-host-test

all: step-test unit-test #self-host-test

run:
	-@racket src/mal.rkt

# Don't run unit tests in main files (as it would start the mal interpreeter)
TEST_SRCS := $(filter-out src/mal.rkt $(wildcard src/step*.rkt),$(wildcard src/*.rkt))

unit-test:
	raco test $(TEST_SRCS)

step-test:
	@echo step 0; cd ../..; make "test^jw-typed-racket^step0" | grep "failing tests"
	@echo step 1; cd ../..; make "test^jw-typed-racket^step1" | grep "failing tests"
	@echo step 2; cd ../..; make "test^jw-typed-racket^step2" | grep "failing tests"
	@echo step 3; cd ../..; make "test^jw-typed-racket^step3" | grep "failing tests"
	@echo step 4; cd ../..; make "test^jw-typed-racket^step4" | grep "failing tests"
	@echo step 5; cd ../..; make "test^jw-typed-racket^step5" | grep "failing tests"
	@echo step 6; cd ../..; make "test^jw-typed-racket^step6" | grep "failing tests"
	@echo step 7; cd ../..; make "test^jw-typed-racket^step7" | grep "failing tests"
	@echo step 8; cd ../..; make "test^jw-typed-racket^step8" | grep "failing tests"
	@echo step 9; cd ../..; make "test^jw-typed-racket^step9" | grep "failing tests"
#	@echo step A - expect 15 soft failures; cd ../..; make "test^jw-typed-racket^stepA" | grep "failing tests"

self-host-test:
	@echo self-host; cd ../..; make MAL_IMPL=jw-typed-racket "test^mal" | grep "TEST RESULTS\|failing tests"

distclean: clean
