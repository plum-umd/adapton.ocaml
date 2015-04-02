(** GrifolaType: Clean rewrite of the original AType signature.

    (Named after this: http://en.wikipedia.org/wiki/Grifola_frondosa).

    Changelog (compared to Yit's original version, circa PLDI 2014):
    =========
    - DatType requires sanitize (for finalizers that do ref-counting)
    - DatType requires string (for graphmovies/logs)
    - Nominal names; Names are DatTypes
    - Notion of an "Articulation point" (generalizes refs and thunks)
    - Simplified/unified memo table interface:
    -- gives nominal interface (missing in PLDI interface)
    -- do not distinguish between nominal and non nominal
    -- do not distinguish between arities (use tupled arguments, not currying?)   
**)

(* module type DatType = Primitives.DatType *)
open Primitives

(* Names for structures defined below. *)
module type NameType = sig
  include DatType
  val nondet : unit -> t   (* System chooses a fresh name; may be nondeterministic. *)
  val gensym : string -> t (* User takes affinity into their own hands when choosing strings.. *)
  val pair   : t -> t -> t (* For creating conjunctive names *)
  val fork   : t -> t * t  (* Input name determines two distinct output names. *)
  val height : t -> int    (* (1) Drawn from a negative binomial distro, (2) Should be fixed for all forked ancestors. *)
  val compare : t -> t -> int (* For Nick. *)
end

(* Articulation-point interface.
   An ArtType is itself a DatType, but enriched with "articulation" (an indirection whose content that varies over time).
   Articulation points are introduced by eliminating (consuming) first-class names.
   ArtType forms an adjuction with a DatType, modulo the consumption of names to create articulations.
   ArtType generalizes the notion of a reference cell and a thunk (thus the new term "articulation").
   ArtType is like Yit's AType.S interface, but stripped down to bare essentials.
*)
module type ArtType = sig
  module Name  : NameType
  module Data  : DatType (* Data content produced by/stored in articulation point. *)
  include DatType

  val cell  : Name.t -> Data.t -> t (* mutable ref cell; supports set operation. *)
  val thunk : Name.t -> (unit -> Data.t) -> t

  val set   : t -> Data.t -> unit (* set: Error unless articulation point is a mutable cell, and caller is outer layer. *)

  val force : t -> Data.t

  (* Namespaces: Memo-table-based recursion ( nominal & classic ). *)
  type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                    mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                    mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

  val mk_mfn : Name.t -> (module DatType with type t = 'arg)
    -> ('arg mfn -> 'arg -> Data.t) -> 'arg mfn
end

(* Pairs of Articulation points, with monadic projection. *)
module type ArtTuple2Type = sig
  module Name : NameType
  module Art1 : ArtType with type Name.t = Name.t
  module Art2 : ArtType with type Name.t = Name.t
  module Art  : ArtType
    with type Name.t = Name.t
    and  type Data.t = Art1.Data.t * Art2.Data.t

  (* projections, monadic: *)
  val split : Name.t -> Art.t -> Art1.t * Art2.t
  val fst   : Name.t -> Art.t -> Art1.t (* First projection, stay within the "Art monad". *)
  val snd   : Name.t -> Art.t -> Art2.t (* Second projection, stay within the "Art monad". *)
end

(* Pairs of Articulation points, with monadic projection. *)
module type ArtTuple3Type = sig
  module Name : NameType
  module Art1 : ArtType with type Name.t = Name.t
  module Art2 : ArtType with type Name.t = Name.t
  module Art3 : ArtType with type Name.t = Name.t
  module Art  : ArtType
    with type Name.t = Name.t
    and  type Data.t = Art1.Data.t * Art2.Data.t * Art3.Data.t

  (* projections, monadic: *)
  val split : Name.t -> Art.t -> Art1.t * Art2.t * Art3.t
  val fst   : Name.t -> Art.t -> Art1.t (* First projection, stay within the "Art monad". *)
  val snd   : Name.t -> Art.t -> Art2.t (* Second projection, stay within the "Art monad". *)
  val get3  : Name.t -> Art.t -> Art3.t (* *)
end

module type ArtLibType = sig
  type lib_id (* distinct id types means distinct libraries. *)

  module MakeArt 
    (Name:NameType) (Data:DatType) 
    : ArtType
    with type Name.t = Name.t
    and  type Data.t = Data.t

  module MakeArtTuple2
    (Name:NameType) 
    (Art1:ArtType with type Name.t = Name.t) 
    (Art2:ArtType with type Name.t = Name.t) 
    : ArtTuple2Type
    with type Name.t = Name.t
    and  type Art1.t = Art1.t and type Art1.Data.t = Art1.Data.t and type Art1.Name.t = Name.t
    and  type Art2.t = Art2.t and type Art2.Data.t = Art2.Data.t and type Art2.Name.t = Name.t

(*
  module MakeArtTuple3
    (Name:NameType) 
    (Art1:ArtType with type Name.t = Name.t) 
    (Art2:ArtType with type Name.t = Name.t) 
    (Art3:ArtType with type Name.t = Name.t) 
    : ArtTuple3Type
    with type Name.t = Name.t
    and  type Art1.t = Art1.t and type Art1.Data.t = Art1.Data.t and type Art1.Name.t = Name.t
    and  type Art2.t = Art2.t and type Art2.Data.t = Art2.Data.t and type Art2.Name.t = Name.t
    and  type Art3.t = Art3.t and type Art3.Data.t = Art3.Data.t and type Art3.Name.t = Name.t
*)

  module Eviction : EvictionType
  module Memotables : MemotablesType

  (* Only needed for SAC, which refreshes globally/manually. This is a
     no-op for Adapton. (Adapton is not global, but done transparently,
     on-demand, when and if affected arts are force'd). 
   *)
  val sac_refresh : unit -> unit
end
