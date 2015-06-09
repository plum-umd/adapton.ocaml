Programmer's Guide for *Incremental Computation with Names* (**ICwN**)
======================================================================

Many sections of **ICwN** describe incremental programming in
**Nominal Adapton** in a somewhat idealized manner.  This document
connects that discussion to the OCaml code of [our implementation](http://github.com/plum-umd/adapton.ocaml/).

Noteworthy modules:
-------------------

 * The [SpreadTree module](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/adapton_structures/SpreadTree.ml) 
contains basic tree and list data structures.

  Ropes versus trees: In the implementation, we refer to binary trees
  as **ropes** when all their elements are restricted to leaf
  positions.  Other binary trees that store elements at internal nodes
  are simply **trees**.  This distiction is absent in **ICwN**.

 * The [Quickhull module](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/test/quickhull.ml)
uses tree and list definitions above to define the quickhull algorithm for convex hulls.

 * The [Experiments module](http://github.com/plum-umd/adapton.ocaml/tree/master/Source/test/experiments.ml) 
defines experiments using the modules above, for basic data structures and algorithms.


ICwN §2: Overview
-------------------

The `list` and `map` definitions in this section of the paper
correspond to the implementation's `list` type and `list_eager_map`
function, both described below.

ICwN §3.1: Mapping
--------------------------

The implementation defines two variants of **mapping** for lists:

 * The `list_map` function lazily maps a list.
 * The `list_eager_map` function eagerly maps a list.

ICwN §3.2: Folding
------------------

* The implementation defines the `rope_reduce` function, which reduces a rope using an associative
  binary operator.  This is similar to a conventional list fold,
  except that the operator is applied to a rope.

ICwN §3.3: Unfolding
--------------------

 * The implementation defines unfolding for **rope** structures.  The `rope_of_list`
   function creates a rope from a list.  An in-order traversal of the
   rope gives the original list elements, in their original order.

