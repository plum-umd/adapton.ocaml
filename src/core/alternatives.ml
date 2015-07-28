(* 
This file contains alternative inputs to Adapton functors
to produce non-adapton results
*)

module type DataS = Data.S

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

  module MakeArt(Name : Name.S)(Data : Data.S)
      : Art.S with type name = Name.t
               and type data = Data.t
    (* and  type t = Data.t node  *)
  = struct
    type name = Name.t
    type data = Data.t

    type 'a node = {
      id : int;
      mutable fn : unit->'a;
    }

    type t = Data.t node

    type 'arg mfn =
      { mfn_data : 'arg -> data ;
        (* Pure recursion. *)
        mfn_art  : 'arg -> t ;
        (* Create a memoized articulation, classically. *)
        mfn_nart : name -> 'arg -> t ;
        (* Create a memoized articulation, nominally. *) }

    let show n = "&"^(string_of_int n.id)
    let pp ff p = Format.pp_print_string ff (show p)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let equal n1 n2 = n1.id = n2.id || n1.fn() = n2.fn() 
    let compare t t' = compare (hash 42 t) (hash 42 t')
    let force { fn; _ } = incr Statistics.Counts.evaluate; fn()
    let sanitize n = n

    let cell _ x = incr Statistics.Counts.create; { id = next_count(); fn = (fun()->x) }
    let set n x = n.fn <- (fun()->x)     
    let thunk _ f = incr Statistics.Counts.create; { id = next_count(); fn = f }


    let mk_mfn (type a)
      _
      (module Arg : DataS with type t = a)
      (user_function: Arg.t mfn -> Arg.t -> data) 
      : Arg.t mfn
    =
      let rec mfn =
      (* incr Statistics.Counts.evaluate;  *)
        { 
          mfn_data = (fun arg -> user_function mfn arg) ;
          mfn_art  = (fun arg -> incr Statistics.Counts.create; { id=next_count(); fn=(fun ()-> user_function mfn arg) } ) ;
          mfn_nart = (fun _ arg -> incr Statistics.Counts.create; { id=next_count(); fn=(fun ()-> user_function mfn arg ) } )
        }
      in mfn
  end

  module ArtLib : ArtLib.S = struct
    type lib_id
    module Memotables = struct
      let register _ = ()
      let print_stats _ = ()
      let fold a _ = a
    end
    module MakeArt = MakeArt
    let sac_refresh () = ()
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

  module MakeArt(Name : Name.S)(Data : Data.S)
    : Art.S with type name = Name.t
             and type data = Data.t =
  struct
    type name = Name.t
    type data = Data.t

    type 'a node = {
      id : int;
      mutable value : 'a;
    }

    type t = Data.t node

    type 'arg mfn =
      { mfn_data : 'arg -> Data.t
      (* Pure recursion. *)
      ; mfn_art  : 'arg -> t
      (* Create a memoized articulation, classically. *)
      ; mfn_nart : Name.t -> 'arg -> t
      (* Create a memoized articulation, nominally. *)
      }

    let show n = "&"^(string_of_int n.id)
    let pp ff p = Format.pp_print_string ff (show p)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let compare t t' = compare (hash 42 t) (hash 42 t')
    let equal n1 n2 = n1.id = n2.id || n1.value = n2.value 
    let force { value; _ } = (* incr Statistics.Counts.evaluate; *) value
    let sanitize n = n

    let cell _ x = { id = next_count(); value = x }
    let set n x = n.value <- x
    let thunk _ f = { id = next_count(); value = f() }


    let mk_mfn (type a)
      _
      (module Arg : DataS with type t = a)
      (user_function: Arg.t mfn -> Arg.t -> data) 
      : Arg.t mfn =
      let rec mfn =
        (* incr Statistics.Counts.evaluate;  *)
        { mfn_data = (fun arg -> user_function mfn arg)
        ; mfn_art  = (fun arg -> cell (Name.nondet()) (user_function mfn arg))
        ; mfn_nart = (fun _ arg -> cell (Name.nondet()) (user_function mfn arg))
        }
      in mfn
  end

  module ArtLib : ArtLib.S = struct
    type lib_id
    module Memotables = struct
      let register _ = ()
      let print_stats _ = ()
      let fold a _ = a
    end

    module MakeArt = MakeArt
    let sac_refresh () = ()
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

  module MakeArt(Name : Name.S)(Data : Data.S)
    : Art.S with type name = Name.t
             and type data = Data.t =
  struct
    type name = Name.t
    type data = Data.t

    type 'a node = {
      id : int;
      mutable thunk : 'a Lazy.t;
    }

    type t = Data.t node

    type 'arg mfn =
      { mfn_data : 'arg -> Data.t
      (* Pure recursion. *)
      ; mfn_art  : 'arg -> t
      (* Create a memoized articulation, classically. *)
      ; mfn_nart : Name.t -> 'arg -> t
      (* Create a memoized articulation, nominally. *)
      }

    let show n = "&"^(string_of_int n.id)
    let pp ff p = Format.pp_print_string ff (show p)
    let hash seed n = Hashtbl.seeded_hash seed n.id
    let compare t t' = compare (hash 42 t) (hash 42 t')
    let equal {id=id1} {id=id2} = id1 = id2
    let force n = (* incr Statistics.Counts.evaluate;  *) failwith "TODO"
    let sanitize n = n

    let cell _ x = { id = next_count(); thunk = lazy x }
    let set n x = n.thunk <- lazy x
    let thunk _ f = { id = next_count(); thunk = lazy ((* incr Statistics.Counts.evaluate; *) f ()) }


    let mk_mfn (type a)
      _
      (module Arg : DataS with type t = a)
      (user_function: Arg.t mfn -> Arg.t -> data) 
      : Arg.t mfn=
      let rec mfn =
      (* incr Statistics.Counts.evaluate;  *)
        { 
          mfn_data = (fun arg -> user_function mfn arg) ;
          mfn_art  = (fun arg -> cell (Name.nondet()) (user_function mfn arg)) ;
          mfn_nart = (fun _ arg -> cell (Name.nondet()) (user_function mfn arg)) ;
        }
      in mfn
  end

  module ArtLib : ArtLib.S = struct
    type lib_id
    module Memotables = struct
      let register _ = ()
      let print_stats _ = ()
      let fold a _ = a
    end

    module MakeArt = MakeArt
    let sac_refresh () = ()
  end
end
                      

module Sac = struct

  module S = SelfAdjMachine
  module MakeArt = S.MakeArt

  module ArtLib : ArtLib.S = struct
    type lib_id
    module Memotables = struct
      let register _ = ()
      let print_stats _ = ()
      let fold a _ = a
    end

    module MakeArt = MakeArt
    let sac_refresh () = S.refresh ()
  end
end
