Adapton
=======

(Nominal) Adapton for OCaml.

Adapton provides tools to build programs that compute incrementally.
See [adapton.org](adapton.org) for more information about Adapton generally, including its contributors, publications and alternative implementations.

Adapton requires:
- OCaml version 4.02.0+
- OPAM packages: core, ppx_deriving

To build Adapton, just run `make`.

To pin Adapton as an OPAM package (under the package name 'adapton'),
just run `make install`.

For a simple and well-documented example,
[go here](sample/spreadsheet.ml). For a more involved, poorly
documented, research-quality use of Adapton, see
[Inc IMP](https://github.com/plum-umd/inc-imp), an incremental
interpreter for a small imperative programming language.

Please contact @labichn if you have any questions or comments about
this Adapton library.
