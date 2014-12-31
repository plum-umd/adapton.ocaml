adapton.ocaml
=============

(Nominal) Adapton in OCaml.  Yit's mecurial repo is here: https://bitbucket.org/khooyp/adapton.ocaml

Currently this library cannot export some key modules:
  Primitives
  Types
  Statistics
so code requiring these must be run within the project, see myocamlbuild for dependency issues.

To run a first experiment:

  make test
  cd script
  ./test-avl.sh

  out/avlropesort.csv contains result data.

To use externally:

  make opam-pin (once)

  in your source:

    open Adapton_lib

    Adapton.Grifola will be available, along with all of adapton_core/
    Adapton.Structures.Spreadtree will be available, along with all of adapton_structures/

    compile your project using opam package 'adapton'.

      with ocamlbuild, in _tags:
        <true>: package(adapton)

To use in toplevel:

  make opam-pin (once)

  ocaml
  #use "topfind";;
  #camlp4o;; <-- optional?
  #thread;;
  #require "Adapton";;
  open Adapton_lib;;

  same availability as in source

After Changing source:

  make opam-reload
