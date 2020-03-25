.PHONY: all run test step-test repl typecheck

all: typecheck lint test step-test

CURRENT_STEP = step3_env.exs

run:
	-mix run $(CURRENT_STEP)

repl:
	-iex -S mix

lint:
	mix credo list --format=oneline -a

test:
	-mix test

typecheck:
	mix dialyzer --quiet

step-test:
	@echo step 0; cd ..; make "test^jw_elixir^step0" | grep "failing tests"
	@echo step 1; cd ..; make "test^jw_elixir^step1" | grep "failing tests"
	@echo step 2; cd ..; make "test^jw_elixir^step2" | grep "failing tests"
	@echo step 3; cd ..; make "test^jw_elixir^step3" | grep "failing tests"
