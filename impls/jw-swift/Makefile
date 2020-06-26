.PHONY: all build clean distclean format run test step-test self-host-test run-debug

all: build lint test step-test self-host-test

TARGET_DIR := ./.build/debug
RUN_EXEC = mal

build:
	swift build
	# For slightly faster runtime, but slower compile time use
	# swift build --configuration release

lint:
	swiftlint

run:
	-@$(TARGET_DIR)/$(RUN_EXEC)

test:
	swift test 2>&1 | grep -v "passed\|started"

step-test:
	-@echo step 0; cd ../..; make "test^jw-swift^step0" | grep "failing tests"
	-@echo step 1; cd ../..; make "test^jw-swift^step1" | grep "failing tests"
	-@echo step 2; cd ../..; make "test^jw-swift^step2" | grep "failing tests"
	-@echo step 3; cd ../..; make "test^jw-swift^step3" | grep "failing tests"
	-@echo step 4; cd ../..; make "test^jw-swift^step4" | grep "failing tests"
	-@echo step 5; cd ../..; make "test^jw-swift^step5" | grep "failing tests"
	-@echo step 6; cd ../..; make "test^jw-swift^step6" | grep "failing tests"
	-@echo step 7; cd ../..; make "test^jw-swift^step7" | grep "failing tests"
	-@echo step 8; cd ../..; make "test^jw-swift^step8" | grep "failing tests"
	-@echo step 9; cd ../..; make "test^jw-swift^step9" | grep "failing tests"
	-@echo step A; cd ../..; make "test^jw-swift^stepA" | grep "failing tests"

self-host-test:
	@cd ../..; make MAL_IMPL=jw-swift "test^mal" | grep "TEST RESULTS\|failing tests"
