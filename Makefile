all: clean
	dune build src/refactor.exe
	cp _build/default/src/refactor.exe .

format:
	dune build @fmt --auto-promote | true

clean:
	rm -f refactor.exe
