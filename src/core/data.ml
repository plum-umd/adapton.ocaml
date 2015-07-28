
module type S =
sig
  (* Abstract signature of data with which Adapton computations compute. *)
  type t
  val equal    : t -> t -> bool
  val compare  : t -> t -> int
  val hash     : int -> t -> int
  val pp       : Format.formatter -> t -> unit
  val show     : t -> string  (* for debugging *)
  val sanitize : t -> t (* makes shallow copy with no finalisers. *)
end

