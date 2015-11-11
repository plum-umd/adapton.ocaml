.PHONY = all,debug,clean,profile

CURDIR=$(shell pwd)
PLIBS=a cma cmi cmo p.cmx p.cmxa
LIBS=a cma cmi cmo cmx cmxa
OCB = ocamlbuild -use-ocamlfind
OPTOPTS = $(OCB) -ocamlopt 'ocamlopt -inline 20'
POPTOPTS = $(OCB) -ocamlopt 'ocamlopt -p -g'
PROFDIR = $(CURDIR)/profile
TIME := $(shell date +"%Y.%m.%d-%H.%M.%S")

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
	$(OCB) src/test/testTrie.native
	./script/test-correctness.sh

test-mem:
	$(OCB) $(OPTOPTS) src/test/sparseTrie.native
	./script/test-mem.sh

src/test/sparseTrie.p.native:
	for ext in $(PLIBS); do \
		$(POPTOPTS) adapton.$$ext || exit 1 ; \
	done
	$(POPTOPTS) src/test/sparseTrie.p.native

profile: src/test/sparseTrie.p.native
	for ext in $(PLIBS); do \
		$(POPTOPTS) adapton.$$ext || exit 1 ; \
	done
	$(POPTOPTS) src/test/sparseTrie.p.native
	./script/test-mem.sh sparseTrie.p.native
	if [ ! -d $(PROFDIR) ]; then mkdir $(PROFDIR); fi
	gprof sparseTrie.p.native > $(PROFDIR)/$(TIME).gprof

install:
	opam pin -y add adapton .

uninstall :
	opam pin -y remove adapton

reinstall: uninstall install

clean:
	ocamlbuild -clean
