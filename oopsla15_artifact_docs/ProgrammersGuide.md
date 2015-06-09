Programmer's Guide for *Incremental Computation with Names* (**ICwN**)
======================================================================

The **ICwN** paper describes incremental programming in **Nominal
Adapton** in a somewhat idealized manner.  This document connects that
discussion to the OCaml code of our implementation, which is publicly
available on github (`http://github.com/plum-umd/adapton.ocaml/`).

Noteworthy modules:
-------------------

The `Source` paths below are relative to the `adapton.ocaml` repository.  The HTTP links point to the `master` branch on `github.com`:

 * The [GrifolaType module (Source/adapton_core/GrifolaType.ml)](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/adapton_core/GrifolaType.ml) gives the OCaml programming API for Nominal Adapton.
 
 * The [SpreadTree module (Source/adapton_structures/SpreadTree.ml)](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/adapton_structures/SpreadTree.ml) uses the API above to define basic tree and list data structures.

  (Ropes versus trees: In the implementation, we refer to binary trees
  as **ropes** when all their elements are restricted to leaf
  positions.  Other binary trees that store elements at internal nodes
  are simply **trees**.  This distiction is absent in **ICwN**.)

 * The [Quickhull module (Source/test/quickhull.ml)](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/test/quickhull.ml)
uses tree and list definitions above to define the quickhull algorithm for convex hulls.

 * The [Experiments module (Source/test/experiments.ml)](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/test/experiments.ml) 
defines experiments using the modules above.

ICwN Sec 2: Overview
----------------------

The `list` and `map` definitions in this section of the paper
correspond to the implementation's `list` type and `list_eager_map`
function.  They are defined in the `SpreadTree` module.

ICwN Sec 3.1: Mapping
--------------------------

The `SpreadTree` module defines two variants of mapping for lists:

 * The `list_map` function lazily maps a list.
 * The `list_eager_map` function eagerly maps a list.

ICwN Sec 3.2: Folding
------------------

* The `SpreadTree` module defines the `rope_reduce` function, which
  reduces a rope using an associative binary operator.  This is
  similar to a conventional list fold, except that the operator is
  applied to a rope.

ICwN Sec 3.3: Unfolding
--------------------

 * The `SpreadTree` module defines unfolding for rope structures.
   The `rope_of_list` function creates a rope from a list.  An
   in-order traversal of the rope gives the original list elements, in
   their original order.

