.PHONY: all clean distclean run test step-test

all: test step-test

CURRENT_STEP = step2_eval.rkt
TEST_RUNNER = tests.rkt

RKT=racket
SRC_DIR := ./src
COMPILE_DIR := ./src/compiled

run:
	-@$(RKT) $(SRC_DIR)/$(CURRENT_STEP)

test:
	-@$(RKT) $(SRC_DIR)/$(TEST_RUNNER)

step-test:
	-@echo step 0; cd ..; make "test^jw-racket^step0" | grep "failing tests"
	-@echo step 1; cd ..; make "test^jw-racket^step1" | grep "failing tests"
	-@echo step 2; cd ..; make "test^jw-racket^step2" | grep "failing tests"

clean:
	$(RM) $(COMPILE_DIR)/*

distclean: clean
