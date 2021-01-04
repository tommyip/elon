default: build

build:
	dune build

run:
	dune exec ./bin/elonc.exe -- $(args)

test:
	dune test

clean:
	dune clean
