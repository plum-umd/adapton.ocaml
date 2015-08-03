module type S =
sig
  type name
  include Data.S
  module Art : Art.S with type data = t
                      and type name = name
end


module type ArtTuple2S = sig
  type name
  module Adpt1 : S with type name = name
  module Adpt2 : S with type name = name
  module Art : Art.S with type name = name
                      and type data = Adpt1.t * Adpt2.t

  (* projections, monadic: *)
  val split : name -> Art.t -> Adpt1.Art.t * Adpt2.Art.t
  val fst   : name -> Art.t -> Adpt1.Art.t (* First projection, stay within the "Art monad". *)
  val snd   : name -> Art.t -> Adpt2.Art.t (* Second projection, stay within the "Art monad". *)
end

module ArtTuple2
    (ArtLib : ArtLib.S)
    (Name   : Name.S) 
    (Adpt1  : S with type name = Name.t) 
    (Adpt2  : S with type name = Name.t)
  : ArtTuple2S with type name = Name.t
                and module Adpt1 = Adpt1
                and module Adpt2 = Adpt2 =
struct
  type name = Name.t
  module Adpt1 = Adpt1
  module Adpt2 = Adpt2
  module Art = ArtLib.MakeArt(Name)(Types.Tuple2(Adpt1)(Adpt2))

  let mfn_fst = Adpt1.Art.mk_mfn (Name.gensym "fst") (module Art) (fun r art -> fst (Art.force art))
  let mfn_snd = Adpt2.Art.mk_mfn (Name.gensym "snd") (module Art) (fun r art -> snd (Art.force art))
      
  let fst nm art = if true then mfn_fst.Adpt1.Art.mfn_art art else mfn_fst.Adpt1.Art.mfn_nart nm art
  let snd nm art = if true then mfn_snd.Adpt2.Art.mfn_art art else mfn_snd.Adpt2.Art.mfn_nart nm art
        
  let split nm x = let nm1,nm2 = Name.fork nm in (fst nm1 x, snd nm2 x)
end
