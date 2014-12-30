#Using ocamlbuild to take care of dependencies, so all targets are .PHONYs
.PHONY: lib test install clean

lib:
	ocamlbuild Source/adapton.a
	ocamlbuild Source/adapton.cma
	ocamlbuild Source/adapton.cmo
	ocamlbuild Source/adapton.cmi

test: lib
	ocamlbuild Source/test/experiments.native
	cp _build/Source/test/experiments.native bin/test_adapton

install: lib
	cp _build/Source/adapton.a lib
	cp _build/Source/adapton.cma lib
	cp _build/Source/adapton.cmo lib
	cp _build/Source/adapton.cmi lib

clean:
	rm -f bin/* lib/*
	ocamlbuild -clean

#clean up more for push to repo
clean-all: clean
	rm -f out/* script/*.gmv