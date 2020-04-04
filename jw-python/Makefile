.PHONY: check all typecheck lint test format
all: check test format typecheck lint step-test host-test

LINT = flake8
TYPECHECK = mypy

SRCDIR = src/
SRCFILES = $(SRCDIR)*.py

format:
	black src/

typecheck:
	$(TYPECHECK) $(SRCDIR)

lint:
	$(LINT) $(SRCDIR)

step-test:
	@echo step 0; cd ..; make "test^jw-python^step0" | grep "failing tests"
	@echo step 1; cd ..; make "test^jw-python^step1" | grep "failing tests"
	@echo step 2; cd ..; make "test^jw-python^step2" | grep "failing tests"
	@echo step 3; cd ..; make "test^jw-python^step3" | grep "failing tests"
	@echo step 4; cd ..; make "test^jw-python^step4" | grep "failing tests"
	@echo step 5; cd ..; make "test^jw-python^step5" | grep "failing tests"
	@echo step 6; cd ..; make "test^jw-python^step6" | grep "failing tests"
	@echo step 7; cd ..; make "test^jw-python^step7" | grep "failing tests"
	@echo step 8; cd ..; make "test^jw-python^step8" | grep "failing tests"
	@echo step 9; cd ..; make "test^jw-python^step9" | grep "failing tests"
	@echo step A; cd ..; make "test^jw-python^stepA" | grep "failing tests"

host-test:
	@echo self-host; cd ..; make MAL_IMPL=jw-python "test^mal" | grep -E "^TEST RESULTS|failing tests"
