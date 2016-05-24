.PHONY: all test clean

all: build

build:
	@stack clean && stack build

test:
	@stack test --test-arguments --format=progress

repl:
	@stack ghci

repl-test:
	@stack ghci cubinhos:spec
