adapton.ocaml
=============

(Nominal) Adapton in OCaml.

Yit's original mecurial repo is here: https://bitbucket.org/khooyp/adapton.ocaml

To build and run the imp interpreter:

   make imp
   ./_build/Sample/Implang/implang.native

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
        <true>: thread
        <true>: package(adapton)

External example: Sample/Trivial

    ocamlbuild -use-ocamlfind find_0_bit.native

To use in toplevel:

    make opam-pin (once)

Toplevel example:

    ocaml
    #use "topfind";;
    #thread;;
    #require "Adapton";;
    open Adapton_lib;;
    module AInt = Adapton.Grifola.Default.MakeArt(Adapton.Key)(Adapton.Types.Int);;
    let a = AInt.cell(AInt.Name.nondet()) 3;;
    let ax2 = AInt.thunk(AInt.Name.nondet())(fun()->(AInt.force a)*2);;
    AInt.force ax2;;
    AInt.set a 7;;
    AInt.force ax2;;

After Changing source:

    make opam-reload



