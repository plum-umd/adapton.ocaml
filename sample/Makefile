.PHONY = all,clean

OCB   = ocamlbuild -use-ocamlfind
FILES = spreadsheet listBasics

all: clean
	for f in $(FILES); do \
		$(OCB) $$f.native || exit 1 ; \
	done

clean:
	ocamlbuild -clean
