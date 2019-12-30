DOCTEST = python3 -m doctest
LINT = flake8
TYPECHECK = mypy

SRCDIR = src/
SRCFILES = $(SRCDIR)*.py

.PHONY: check all typecheck lint test format
all: format check test
check: typecheck lint doctest

doctest:
	$(DOCTEST) $(SRCFILES)

doctestv:
	$(DOCTEST) -v $(SRCFILES)

typecheck:
	$(TYPECHECK) $(SRCDIR)

lint:
	$(LINT) $(SRCDIR)

test: step0 step1 step2 step3 step4

format:
	black --target-version py37 src/

step0:
	cd ..; make "test^jw-python^step0" | grep ": failing tests"

step1:
	cd ..; make "test^jw-python^step1" | grep ": failing tests"

step2:
	cd ..; make "test^jw-python^step2" | grep ": failing tests"

step3:
	cd ..; make "test^jw-python^step3" | grep ": failing tests"

step4:
	cd ..; make "test^jw-python^step4" | grep ": failing tests"
