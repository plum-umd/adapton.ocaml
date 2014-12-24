all: Adapton experiments.native

Adapton:
	ocamlbuild Source/Adapton.cma

experiments.native:
	ocamlbuild Source/test/experiments.native