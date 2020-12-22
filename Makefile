all: clean
	dune build refactor.exe
	cp _build/default/refactor.exe .

clean:
	rm -f refactor.exe
