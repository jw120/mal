.PHONY: all run test step-test repl

all: test step-test

CURRENT_STEP = step1_read_print.exs

run:
	-mix run $(CURRENT_STEP)

repl:
	-iex -S mix

test:
	-mix test

step-test:
	@echo step 0; cd ..; make "test^jw-racket^step0" | grep "failing tests"
	@echo step 1; cd ..; make "test^jw-racket^step1" | grep "failing tests"
