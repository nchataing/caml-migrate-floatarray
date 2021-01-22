all: clean
	dune build src/cmd.exe
	cp _build/default/src/cmd.exe refactor

format:
	dune build @fmt --auto-promote | true

clean:
	rm -f refactor
