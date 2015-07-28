module type S =
sig
  module Mfn : MemoFun.S
  val length : unit -> int
  val numadd : unit -> int
  val numrem : unit -> int
  val fold   : 'a -> ('a -> 'a) -> 'a
end

module type REGISTRY =
sig
  val register : (module S) -> unit
  val print_stats : out_channel -> unit
  val fold : 'a -> ('a -> (module S) -> 'a) -> 'a
end
