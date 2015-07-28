module type S =
sig
  type lib_id (* distinct id types means distinct libraries. *)

  module MakeArt(Name : Name.S)(Data : Data.S)
    : Art.S with type name = Name.t
             and type data = Data.t

  module Memotables : MemoTable.REGISTRY

  (* Only needed for SAC, which refreshes globally/manually. This is a
     no-op for Adapton. (Adapton is not global, but done transparently,
     on-demand, when and if affected arts are force'd). 
   *)
  val sac_refresh : unit -> unit
end


