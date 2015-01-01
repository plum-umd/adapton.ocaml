adapton.ocaml
=============

(Nominal) Adapton in OCaml.  Yit's mecurial repo is here: https://bitbucket.org/khooyp/adapton.ocaml

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

    for example: Source/Trivial

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



