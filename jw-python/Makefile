DOCTEST = python3 -m doctest
LINT = pylint
TYPECHECK = mypy

SRCDIR = src/
SRCFILES = $(SRCDIR)/*.py

.PHONY: check all typecheck lint test
all: check test
check: typecheck lint doctest

doctest:
	$(DOCTEST) $(SRCFILES)

doctestv:
	$(DOCTEST) -v $(SRCFILES)

typecheck:
	$(TYPECHECK) $(SRCDIR)

lint:
	$(LINT) $(SRCDIR)

test: step0 step1

step0:
	cd ..; make "test^jw-python^step0" | grep ": failing tests"

step1:
	cd ..; make "test^jw-python^step1" | grep ": failing tests"
