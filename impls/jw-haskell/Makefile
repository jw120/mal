.PHONY: all format lint build run repl test step-test host-test

all: format lint build test step-test host-test

format:
	brittany --write-mode=inplace app/*.hs src/*.hs test/*.hs

lint:
	hlint app/*.hs src/*.hs test/*.hs

build:
	stack build

run:
	stack exec mal

test:
	stack test

step-test:
	@echo step 0; cd ..; make "test^jw-haskell^step0" | grep "failing tests"
	@echo step 1; cd ..; make "test^jw-haskell^step1" | grep "failing tests"
	@echo step 2; cd ..; make "test^jw-haskell^step2" | grep "failing tests"
	@echo step 3; cd ..; make "test^jw-haskell^step3" | grep "failing tests"
	@echo step 4; cd ..; make "test^jw-haskell^step4" | grep "failing tests"
	@echo step 5; cd ..; make "test^jw-haskell^step5" | grep "failing tests"
	@echo step 6; cd ..; make "test^jw-haskell^step6" | grep "failing tests"
	@echo step 7; cd ..; make "test^jw-haskell^step7" | grep "failing tests"
	@echo step 8; cd ..; make "test^jw-haskell^step8" | grep "failing tests"
	@echo step 9; cd ..; make "test^jw-haskell^step9" | grep "failing tests"
	@echo step A; cd ..; make "test^jw-haskell^stepA" | grep "failing tests"

host-test:
	@echo self-host; cd ..; make MAL_IMPL=jw-haskell "test^mal" | grep -E "^TEST RESULTS|failing tests"
