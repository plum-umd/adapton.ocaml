#Using myocamlbuild to take care of dependencies, so all targets are .PHONYs
.PHONY: lib test opam-pin opam-remove clean clean-all

#coordinate this with adapton.install for opam use
LIBS=a cma cmo cmi cmx cmxa

lib:
	for ext in $(LIBS); do \
		ocamlbuild Source/adapton.$$ext; \
	done

#ocamlbuild will put an alias to binaries in the root directory
test:
	ocamlbuild Source/test/experiments.native

opam-pin:
	opam pin add adapton .

opam-remove:
	opam pin remove adapton

clean:
	ocamlbuild -clean

#clean up more for push to repo
clean-all: clean
	rm -f out/* script/*.gmv