.PHONY: check all typecheck lint test format
all: check test
check: format typecheck lint doctest

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

test: step0 step1 step2 step3 step4 step5 step6 step7 step8 step9 stepA

step0:
	cd ..; make "test^jw-python^step0" | grep "failing tests"

step1:
	cd ..; make "test^jw-python^step1" | grep "failing tests"

step2:
	cd ..; make "test^jw-python^step2" | grep "failing tests"

step3:
	cd ..; make "test^jw-python^step3" | grep "failing tests"

step4:
	cd ..; make "test^jw-python^step4" | grep "failing tests"

step5:
	cd ..; make "test^jw-python^step5" | grep "failing tests"

step6:
	cd ..; make "test^jw-python^step6" | grep "failing tests"

step7:
	cd ..; make "test^jw-python^step7" | grep "failing tests"

step8:
	cd ..; make "test^jw-python^step8" | grep "failing tests"

step9:
	cd ..; make "test^jw-python^step9" | grep "failing tests"

stepA:
	cd ..; make "test^jw-python^stepA" | grep "failing tests"
