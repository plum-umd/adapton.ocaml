#Using myocamlbuild to take care of dependencies, so all targets are .PHONYs
.PHONY: lib test opam-pin opam-remove clean clean-all update

#coordinate this with adapton.install for opam use
LIBS=a cma cmo cmi cmx cmxa

lib:
	for ext in $(LIBS); do \
		ocamlbuild Source/adapton_lib.$$ext; \
	done

update:
	git pull
	make opam-remove
	make opam-pin

imp:
	ocamlbuild Sample/Implang/implang.native

#ocamlbuild will put an alias to binaries in the root directory
test:
	ocamlbuild Source/test/experiments.native

test-db:
	ocamlbuild -cflag '-g' -lflag '-g' Source/test/experiments.byte

# this doesn't work yet
test-log:
	ocamlbuild -cflags -ppopt,"-DADAPTON_LOG" Source/test/experiments.native -verbose 1

opam-pin:
	opam pin add adapton .

opam-remove:
	opam pin remove adapton

opam-reload: opam-remove opam-pin

clean:
	ocamlbuild -clean

#clean up more for push to repo
clean-all: clean
	rm -f *.gmv out/*.gmv out/*.csv script/*.gmv
