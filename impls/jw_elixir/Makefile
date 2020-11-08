.PHONY: all run repl format docs lint test step-test host-test repl typecheck

all: typecheck lint format test step-test host-test

CURRENT_STEP = stepA_mal.exs

run:
	-mix run $(CURRENT_STEP)

repl:
	-iex -S mix

format:
	mix format

docs:
	mix docs

lint:
	mix credo list --format=oneline -a

test:
	-mix test

typecheck:
	mix dialyzer --quiet

step-test:
	@echo step 0; cd ../..; make "test^jw_elixir^step0" | grep "failing tests"
	@echo step 1; cd ../..; make "test^jw_elixir^step1" | grep "failing tests"
	@echo step 2; cd ../..; make "test^jw_elixir^step2" | grep "failing tests"
	@echo step 3; cd ../..; make "test^jw_elixir^step3" | grep "failing tests"
	@echo step 4; cd ../..; make "test^jw_elixir^step4" | grep "failing tests"
	@echo step 5; cd ../..; make "test^jw_elixir^step5" | grep "failing tests"
	@echo step 6; cd ../..; make "test^jw_elixir^step6" | grep "failing tests"
	@echo step 7; cd ../..; make "test^jw_elixir^step7" | grep "failing tests"
	@echo step 8; cd ../..; make "test^jw_elixir^step8" | grep "failing tests"
	@echo step 9; cd ../..; make "test^jw_elixir^step9" | grep "failing tests"
	@echo step A; cd ../..; make "test^jw_elixir^stepA" | grep "failing tests"

host-test:
	@echo self-host; cd ../..; make MAL_IMPL=jw_elixir "test^mal" | grep -E "^TEST RESULTS|failing tests"
