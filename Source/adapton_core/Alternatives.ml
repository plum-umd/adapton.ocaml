(* 
This file contains alternative inputs to Adapton functors
to produce non-adapton results
*)


open Primitives

(* a minimal alternative to Grifola.Make, recalculating at each force *)
module LazyNonInc = struct
  (* TODO: consider using the name as the id instead of a count *)
  let counter = ref 0
  let next_count = 
    let ret = counter in
    counter := !counter+1;
    ret

  module MakeArt
    (Name:GrifolaType.NameType) (Data:DatType)
    : GrifolaType.ArtType
    with type Name.t = Name.t
    and  type Data.t = Data.t
    (* and  type t = Data.t node  *)
  = struct
    module Name = Name
    module Data = Data

    type 'a lazy_node = {
      id : int;
      mutable fn : unit->'a;
    }

    type t = Data.t lazy_node

    type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                      mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                      mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

    let string n = "&"^(string_of_int n.id)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let equal n1 n2 = n1.id = n2.id || n1.fn() = n2.fn() 
    let force { fn; _ } = (* incr Statistics.Counts.evaluate; *) fn()
    let sanitize n = n

    let cell _ x = { id = !next_count; fn = (fun()->x) }
    let set n x = n.fn <- (fun()->x)     
    let thunk _ f = { id = !next_count; fn = f }


    let mk_mfn (type a)
      _
      (module Arg : DatType with type t = a)
      (user_function: Arg.t mfn -> Arg.t -> Data.t) 
      : Arg.t mfn
    =
      let rec mfn =
      (* incr Statistics.Counts.evaluate;  *)
        { 
          mfn_data = (fun arg -> user_function mfn arg) ;
          mfn_art  = (fun arg -> cell (Name.nondet()) (user_function mfn arg)) ;
          mfn_nart = (fun _ arg -> cell (Name.nondet()) (user_function mfn arg)) ;
        }
      in mfn
  end

  module ArtLib : GrifolaType.ArtLibType = struct
    type lib_id
    module Memotables = struct
      let register _ = ()
      let print_stats _ = ()
      let fold a _ = a
    end
    module Eviction = struct
      let flush _ = ()
      let set_policy _ = ()
      let set_limit _ = ()
    end

    module MakeArt = MakeArt
    module MakeArtTuple2
      (Name:GrifolaType.NameType) 
      (Art1:GrifolaType.ArtType with type Name.t = Name.t) 
      (Art2:GrifolaType.ArtType with type Name.t = Name.t) 
      : GrifolaType.ArtTuple2Type
      with type Name.t = Name.t
      and  type Art1.t = Art1.t and type Art1.Data.t = Art1.Data.t and type Art1.Name.t = Name.t
      and  type Art2.t = Art2.t and type Art2.Data.t = Art2.Data.t and type Art1.Name.t = Name.t =
    struct
      module Name = Name
      module Art1 = Art1
      module Art2 = Art2
      module Art = MakeArt(Name)(AdaptonTypes.Tuple2(Art1.Data)(Art2.Data))        
      
      let mfn_fst = Art1.mk_mfn (Name.gensym "fst") (module Art) (fun r art -> fst (Art.force art))
      let mfn_snd = Art2.mk_mfn (Name.gensym "snd") (module Art) (fun r art -> snd (Art.force art))

      let fst nm art = if true then mfn_fst.Art1.mfn_art art else mfn_fst.Art1.mfn_nart nm art
      let snd nm art = if true then mfn_snd.Art2.mfn_art art else mfn_snd.Art2.mfn_nart nm art

      let split nm x = let nm1,nm2 = Name.fork nm in (fst nm1 x, snd nm2 x)
    end
  end
end
