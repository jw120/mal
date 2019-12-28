LINT = pylint
TYPECHECK = mypy

SRCDIR = src/

.PHONY: all typecheck lint test
all: typecheck lint test

typecheck:
	$(TYPECHECK) $(SRCDIR)

lint:
	$(LINT) $(SRCDIR)

test:
	cd ..; make "test^jw-python^step0"
