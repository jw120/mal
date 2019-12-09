.PHONY: default all format lint
default:
	stack build

all: format lint

format:
	brittany --write-mode=inplace app/*.hs src/*.hs test/*.hs

lint:
	hlint app/*.hs src/*.hs test/*.hs
