
(* Articulation-point interface.
   An Art.S is itself a Data.S, but enriched with "articulation" (an indirection whose content that varies over time).
   Articulation points are introduced by eliminating (consuming) first-class names.
   Art.S forms an adjuction with a Data.S, modulo the consumption of names to create articulations.
   Art.S generalizes the notion of a reference cell and a thunk (thus the new term "articulation").
   Art.S is like Yit's AType.S interface, but stripped down to bare essentials.
*)
module type S =
sig
  type name
  type data
  include Data.S

  (* mutable ref cell; supports set operation. *)
  val cell  : name -> data -> t
  val thunk : name -> (unit -> data) -> t

  (* set: Error unless articulation point is a mutable cell, and caller is outer layer. *)
  val set   : t -> data -> unit

  val force : t -> data

  (* Namespaces: Memo-table-based recursion ( nominal & classic ). *)
  type 'arg mfn = { mfn_data : 'arg -> data ;      (* Pure recursion. *)
                    mfn_art  : 'arg -> t ;         (* Create a memoized articulation, classically. *)
                    mfn_nart : name -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

  val mk_mfn : name -> (module Data.S with type t = 'arg) -> ('arg mfn -> 'arg -> data) -> 'arg mfn
end
