
all : benchffs.native

debug : benchffs.d.byte

clean :
	ocamlbuild -clean

%.native %.byte :
	ocamlbuild  -use-ocamlfind -no-links $@ --
