.PHONY: all clean distclean run test step-test self-host-test

all: step-test #self-host-test

CURRENT_STEP = step1_read_print.rkt

RKT=racket
RACO_TEST=raco test
SRC_DIR := ./src

run:
	-@$(RKT) $(SRC_DIR)/$(CURRENT_STEP)

test:
	@$(RACO_TEST) $(SRC_DIR)

step-test:
	@echo step 0; cd ../..; make "test^jw-racket^step0" | grep "failing tests"
	@echo step 1; cd ../..; make "test^jw-racket^step1" | grep "failing tests"
	@echo step 2; cd ../..; make "test^jw-racket^step2" | grep "failing tests"
#	@echo step 3; cd ../..; make "test^jw-racket^step3" | grep "failing tests"
#	@echo step 4; cd ../..; make "test^jw-racket^step4" | grep "failing tests"
#	@echo step 5; cd ../..; make "test^jw-racket^step5" | grep "failing tests"
#	@echo step 6; cd ../..; make "test^jw-racket^step6" | grep "failing tests"
#	@echo step 7; cd ../..; make "test^jw-racket^step7" | grep "failing tests"
#	@echo step 8; cd ../..; make "test^jw-racket^step8" | grep "failing tests"
#	@echo step 9; cd ../..; make "test^jw-racket^step9" | grep "failing tests"
#	@echo step A - expect 15 soft failures; cd ../..; make "test^jw-racket^stepA" | grep "failing tests"

self-host-test:
	@echo self-host; cd ../..; make MAL_IMPL=jw-racket "test^mal" | grep "TEST RESULTS\|failing tests"

distclean: clean
