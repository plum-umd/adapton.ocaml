.PHONY = all,debug,clean

OCB = ocamlbuild -use-ocamlfind
OPTOPTS = $(OCB) -ocamlopt 'ocamlopt -inline 20'
POPTOPTS = $(OCB) -ocamlopt 'ocamlopt -p -g'
LIBS=a cma cmi cmo cmx cmxa

all: clean
	for ext in $(LIBS); do \
		$(OCB) adapton.$$ext || exit 1 ; \
	done

test: clean
	$(OCB) src/test/experiments.native
	$(OCB) src/test/testTrie.native
	cd script && ./test-oopsla15.sh
	./testTrie.native

test-correctness: clean
	$(OCB) src/test/experiments.native
	./script/test-correctness.sh

install:
	opam pin -y add adapton .

uninstall :
	opam pin -y remove adapton

reinstall: uninstall install

clean:
	ocamlbuild -clean
