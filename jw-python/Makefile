DOCTEST = python3 -m doctest
LINT = pylint
TYPECHECK = mypy

SRCDIR = src/
SRCFILES = $(SRCDIR)/*.py

.PHONY: all typecheck lint test
all: typecheck lint doctest test

doctest:
	$(DOCTEST) $(SRCFILES)

doctestv:
	$(DOCTEST) -v $(SRCFILES)

typecheck:
	$(TYPECHECK) $(SRCDIR)

lint:
	$(LINT) $(SRCDIR)

test:
	echo step0; cd ..; make "test^jw-python^step0"
	echo step1; cd ..; make "test^jw-python^step1"

