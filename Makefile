default: build

build:
	dune build

run:
	dune exec ./bin/elonc.exe -- $(args)

clean:
	dune clean
