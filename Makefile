all: clean
	dune build src/refactor.exe
	cp _build/default/src/refactor.exe .

clean:
	rm -f refactor.exe
