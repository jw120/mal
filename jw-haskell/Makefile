all: format lint

# Create all our binaries and copy into the bin directory
bin/step0_repl:	app/step0_repl.hs src/*.hs
	stack build --local-bin-path bin --copy-bins

bin/step1_read_print:	app/step1_read_print.hs src/*.hs
	stack build --local-bin-path bin --copy-bins

bin/step2_eval:	app/step2_eval.hs src/*.hs
	stack build --local-bin-path bin --copy-bins

bin/step3_env:	app/step3_env.hs src/*.hs
	stack build --local-bin-path bin --copy-bins

format:
	brittany --write-mode=inplace app/*.hs src/*.hs

lint:
	hlint app/*.hs src/*.hs
