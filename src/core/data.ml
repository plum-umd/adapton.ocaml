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

module type P =
sig
  type 'a t
  val equal    : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val compare  : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val hash     : (int -> 'a -> int) -> int -> 'a t -> int
  val pp       : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val show     : (Format.formatter -> 'a -> unit) -> 'a t -> string
  val sanitize : ('a -> 'a) -> 'a t -> 'a t
end
