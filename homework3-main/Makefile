.PHONY: all clean utop test

all: main.exe test.exe

main.exe:
	dune build main.exe

test.exe:
	dune build test.exe

utop:
	dune utop

clean:
	dune clean

test: test.exe
	./_build/default/test.exe