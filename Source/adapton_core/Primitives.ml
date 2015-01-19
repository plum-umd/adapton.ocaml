
module type DatType = sig
  (* Abstract signature of data with which Adapton computations compute. *)
  type t
  val equal    : t -> t -> bool
  val hash     : int -> t -> int
  val string   : t -> string  (* for debugging *)
  val sanitize : t -> t (* makes shallow copy with no finalisers. *)
end

module type ArgType    = DatType (* Argument for memoization. *)
module type ResultType = DatType (* Result of memoization. *)

module type MfnType = sig (* Abstract memoized function. *)
  val id   : int (* For hashing and equality. *)
  val name : unit -> string (* For debugging. *)
  module Name : DatType (* for hashing and equality *)
  module Arg  : DatType (* for hashing and equality *)
  type res
end

module type MemotableType = sig
  module Mfn : MfnType
  val length : unit -> int
  val numadd : unit -> int
  val numrem : unit -> int
  val fold   : 'a -> ('a -> Viz.node -> 'a) -> 'a
end

(* Global memo table registry. *)
(* Provides a generic interface for interacting with memotables in the global registry. *)
(* Unlike the programming interface, this interface does not produce node handles. *)
module type MemotablesType = sig
  val register : (module MemotableType) -> unit
  val print_stats : out_channel -> unit
  val fold : 'a -> ('a -> (module MemotableType) -> 'a) -> 'a
end

module type EvictionType = sig
  val flush      : unit -> unit
  val set_policy : string -> unit
  val set_limit  : int option -> unit
end

(* http://en.wikipedia.org/wiki/Find_first_set *)
let rec ffs x =
  if x = 0 then 0
  else
    let rec loop t r =
      if (x land t) = 0 then r
      else loop (t lsl 1) (r + 1)
    in loop 1 0
