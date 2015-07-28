module type S =
sig
  module Name : Name.S
  module Arg  : Data.S
  type res
  val id   : int (* For hashing and equality. *)
  val name : unit -> string (* For debugging. *)
end
