all: Adapton experiments

Adapton:
	ocamlbuild Source/Adapton.cma

experiments:
	ocamlbuild Source/test/experiments.native
	cp _build/Source/test/experiments.native bin

clean:
	rm bin/*.native
	ocamlbuild -clean