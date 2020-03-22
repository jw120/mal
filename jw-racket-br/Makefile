.PHONY: all clean distclean run test step-test self-host-test

all: step-test
#test step-test self-host-test

CURRENT_STEP = stepA_mal.rkt
TEST_RUNNER = tests.rkt

RKT=racket
REPL_RKT=brmal/repl.rkt
RACO_TEST=raco test
SRC_DIR := ./brmal
COMPILE_DIR := ./brmal/compiled
#FILES_WITH_TESTS := src/env.rkt src/reader.rkt src/utils.rkt

run:
	-@$(RKT) $(REPL_RKT) -d

#test:
#	@$(RACO_TEST) $(FILES_WITH_TESTS)

step-test:
	@echo step 0; cd ..; make "test^jw-racket-br^step0" | grep "failing tests"
#@echo step 1; cd ..; make "test^jw-racket-br^step1" | grep "failing tests"
	@echo step 2; cd ..; make "test^jw-racket-br^step2" | grep "failing tests"
#@echo step 3; cd ..; make "test^jw-racket-br^step3" | grep "failing tests"
#@echo step 4; cd ..; make "test^jw-racket-br^step4" | grep "failing tests"
#@echo step 5; cd ..; make "test^jw-racket-br^step5" | grep "failing tests"
#@echo step 6; cd ..; make "test^jw-racket-br^step6" | grep "failing tests"
#@echo step 7; cd ..; make "test^jw-racket-br^step7" | grep "failing tests"
#@echo step 8; cd ..; make "test^jw-racket-br^step8" | grep "failing tests"
#@echo step 9; cd ..; make "test^jw-racket-br^step9" | grep "failing tests"
#@echo step A - expect 15 soft failures; cd ..; make "test^jw-racket-br^stepA" | grep "failing tests"

#self-host-test:
#	@echo self-host; cd ..; make MAL_IMPL=jw-racket-br "test^mal" | grep "TEST RESULTS\|failing tests"

clean:
	$(RM) $(COMPILE_DIR)/*

distclean: clean
