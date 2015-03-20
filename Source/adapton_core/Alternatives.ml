(* 
This file contains alternative inputs to Adapton functors
to produce non-adapton results
*)

(* module S = SelfAdjMachine *) (* <<<<<<<<<<<<< !!!!!!!!!! *)

open Primitives

(*
  a minimal alternative to Grifola.Make
  stores a lazy fn but realculates it at
  every force to maintain incremental 
  correctness
*)
module LazyRecalc = struct
  (* TODO: consider using the name as the id instead of a count *)
  let counter = ref 0
  let next_count ()= 
    let ret = !counter in
    counter := ret+1;
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

    type 'a node = {
      id : int;
      mutable fn : unit->'a;
    }

    type t = Data.t node

    type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                      mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                      mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

    let string n = "&"^(string_of_int n.id)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let equal n1 n2 = n1.id = n2.id || n1.fn() = n2.fn() 
    let force { fn; _ } = (* incr Statistics.Counts.evaluate; *) fn()
    let sanitize n = n

    let cell _ x = { id = next_count(); fn = (fun()->x) }
    let set n x = n.fn <- (fun()->x)     
    let thunk _ f = { id = next_count(); fn = f }


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

(*
  a minimal alternative to Grifola.Make
  calculates its value on creation and 
  returns the same value every force

  This will return INCORRECT data after
  an incremental change, only used for
  benchmarking purposes
*)
module EagerNonInc = struct
  (* TODO: consider using the name as the id instead of a count *)
  let counter = ref 0
  let next_count ()= 
    let ret = !counter in
    counter := ret+1;
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

    type 'a node = {
      id : int;
      mutable value : 'a;
    }

    type t = Data.t node

    type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                      mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                      mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

    let string n = "&"^(string_of_int n.id)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let equal n1 n2 = n1.id = n2.id || n1.value = n2.value 
    let force { value; _ } = (* incr Statistics.Counts.evaluate; *) value
    let sanitize n = n

    let cell _ x = { id = next_count(); value = x }
    let set n x = n.value <- x
    let thunk _ f = { id = next_count(); value = f() }


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

(*
  a minimal alternative to Grifola.Make
  calculates its value on force and 
  caches it for use later

  This will return INCORRECT data after
  an incremental change, only used for
  benchmarking purposes
*)
module LazyNonInc = struct
  (* TODO: consider using the name as the id instead of a count *)
  let counter = ref 0
  let next_count ()= 
    let ret = !counter in
    counter := ret+1;
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

    type 'a node = {
      id : int;
      mutable thunk : 'a Lazy.t;
    }

    type t = Data.t node

    type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                      mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                      mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

    let string n = "&"^(string_of_int n.id)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let equal {id=id1} {id=id2} = id1 = id2
    let force n = (* incr Statistics.Counts.evaluate;  *) failwith "TODO"
    let sanitize n = n

    let cell _ x = { id = next_count(); thunk = lazy x }
    let set n x = n.thunk <- lazy x
    let thunk _ f = { id = next_count(); thunk = lazy ((* incr Statistics.Counts.evaluate; *) f ()) }


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
                      
(*
  a minimal alternative to Grifola.Make
  calculates its value on force and 
  caches it for use later

  This will return INCORRECT data after
  an incremental change, only used for
  benchmarking purposes
*)
module SacImpl = struct

  (* TODO: consider using the name as the id instead of a count *)
  let counter = ref 0
  let next_count ()= 
    let ret = !counter in
    counter := ret+1;
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

    type 'a node = {
      id : int;
    }

    type t = Data.t node

    type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                      mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                      mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

    let string n = "&"^(string_of_int n.id)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let equal {id=id1 } {id=id2 } = id1 = id2
    let force n = (* incr Statistics.Counts.evaluate;  *) failwith "TODO"                                                                      
    let sanitize n = n

    let cell _ x = { id = next_count() }
    let set n x = failwith "TODO"
    let thunk _ f = ((* incr Statistics.Counts.evaluate; *) f ()) ; { id = next_count() }


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
