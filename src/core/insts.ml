(* ArtLib instances (user code shouldn't use these, except for
 * the purpose of developing Adapton). To help debug user code,
 * we should have a high-level toggle to switch to from-scratch
 * execution. What's the right way to do this?
 *)

module Structural : ArtLib.S =
struct
  module C = Engine.Make(struct
    include Engine.Default_params
    let disable_names  = true
    let generative_ids = true
    end)
  include C.ArtLib
end

module FromScratch : ArtLib.S =
struct
  include Alternatives.LazyRecalc.ArtLib
  let sac_refresh () = ()
end

module Nominal : ArtLib.S =
struct
  include Engine.Make(Engine.Default_params)
  let sac_refresh () = ()
  type lib_id
end
