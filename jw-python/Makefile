DOCTEST = python3 -m doctest
LINT = pylint
TYPECHECK = mypy

SRCDIR = src/
SRCFILES = $(SRCDIR)/*.py

.PHONY: check all typecheck lint test
check: typecheck lint doctest
all: check

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
	echo step0; cd ..; make "test^jw-python^step0" | grep "failing tests"

step1:
	echo step1; cd ..; make "test^jw-python^step1"
