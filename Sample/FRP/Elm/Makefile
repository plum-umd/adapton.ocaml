
example.native: example.ml
	ocamlbuild -use-ocamlfind example.native

go: example.native
	./example.native

clean:
	ocamlbuild -clean

