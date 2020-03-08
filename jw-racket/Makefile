.PHONY: all clean distclean run test step-test

all: test step-test

CURRENT_STEP = step8_macros.rkt
TEST_RUNNER = tests.rkt

RKT=racket
RACO_TEST=raco test
SRC_DIR := ./src
COMPILE_DIR := ./src/compiled
FILES_WITH_TESTS := src/env.rkt src/reader.rkt src/utils.rkt

run:
	-@$(RKT) $(SRC_DIR)/$(CURRENT_STEP)

test:
	@$(RACO_TEST) $(FILES_WITH_TESTS)

step-test:
	@echo step 0; cd ..; make "test^jw-racket^step0" | grep "failing tests"
	@echo step 1; cd ..; make "test^jw-racket^step1" | grep "failing tests"
	@echo step 2; cd ..; make "test^jw-racket^step2" | grep "failing tests"
	@echo step 3; cd ..; make "test^jw-racket^step3" | grep "failing tests"
	@echo step 4; cd ..; make "test^jw-racket^step4" | grep "failing tests"
	@echo step 5; cd ..; make "test^jw-racket^step5" | grep "failing tests"
	@echo step 6; cd ..; make "test^jw-racket^step6" | grep "failing tests"
	@echo step 7; cd ..; make "test^jw-racket^step7" | grep "failing tests"
	@echo step 8; cd ..; make "test^jw-racket^step8" | grep "failing tests"

clean:
	$(RM) $(COMPILE_DIR)/*

distclean: clean
