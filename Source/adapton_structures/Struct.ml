(** Struct: an experimental alternative to spreadtrees

*)
open Adapton_core
open Primitives
open GrifolaType
module Types = AdaptonTypes
module Statistics = AdaptonStatistics

(* module type StructType = sig
  module ArtLib : ArtLibType
  module Name : NameType
  module Data : DatType

  (* 1+R:(a)+D:(d,s)+A:(n,a)+B:(n,a,s)+C:(s) *)
  type 'art art_struct = [ (* articulated struct. *)
  | `Nil
  | `Ready of 'art
  | `Data of Data.t * 'art art_struct
  | `Art of Name.t * 'art
  | `Branch of Name.t * 'art * 'art art_struct
  | `Continue of 'art art_struct   
  ]

  module rec Datastruct : sig
    module Data : DatType
    module Art  : ArtType
  end
    with type Data.t    = Datastruct.Art.t art_struct
    and type Art.Data.t = Datastruct.Art.t art_struct
    and type Art.Name.t = Name.t

end
 *)
module type SParamsType = sig
  val max_unarticulated : int
  val min_value_branched : int
end

module MakeCommonStruct
  (ArtLib : ArtLibType)
  (Name   : NameType)
  (Data   : DatType)
  (Params : SParamsType)
(*   : StructType with type
    ArtLib.lib_id = ArtLib.lib_id
    and  type Name.t = Name.t
    and  type Data.t = Data.t 
 *)=struct
  module ArtLib = ArtLib
  module Name = Name
  module Data = Data

  type 'art art_struct = [ (* articulated sequence. *)
  | `Nil
  | `Ready of 'art
  | `Data of Data.t * 'art art_struct
  | `Art of Name.t * 'art
  | `Branch of Name.t * 'art * 'art art_struct
    (* constrain `Continue to branches? *)  
  | `Continue of 'art art_struct
  ]

  module rec Datastruct : sig
    module Data : DatType
    module Art  : ArtType
  end
    with type Data.t    = Datastruct.Art.t art_struct
    and type Art.Data.t = Datastruct.Art.t art_struct
    and type Art.Name.t = Name.t
  =struct
    module Data = struct
      type t = Datastruct.Art.t art_struct

      let rec string x =
        match x with
        | `Nil -> "Nil"
        | `Ready(a) -> "Ready("^(Datastruct.Art.string a)^")"
        | `Data(x,xs) -> "Data("^(Data.string x)^","^(string xs)^")"
        | `Art(n,a) -> "Art("^(Name.string n)^","^(Datastruct.Art.string a)^")"
        | `Branch(n,a,xs) -> "Branch("^(Name.string n)^","^(Datastruct.Art.string a)^","^(string xs)^")"
        | `Continue(xs) -> "Continue("^(string xs)^")"

      let rec hash seed x =
        match x with
        | `Nil -> Hashtbl.seeded_hash seed `Nil
        | `Ready(a) -> Datastruct.Art.hash seed a
        | `Data(x,xs) -> Data.hash (hash seed xs) x
        | `Art(n,a) -> Name.hash (Datastruct.Art.hash seed a) n
        | `Branch(n,a,xs) -> Name.hash (Datastruct.Art.hash (hash seed xs) a) n
        | `Continue(xs) -> hash seed xs

      let rec equal xs ys =
        match xs, ys with
        | `Nil, `Nil -> true
        | `Ready(a1), `Ready(a2) -> Datastruct.Art.equal a1 a2
        | `Data(x1,xs1), `Data(x2,xs2) -> Data.equal x1 x2 && equal xs1 xs2
        | `Art(n1,a1), `Art(n2,a2) -> Name.equal n1 n2 && Datastruct.Art.equal a1 a2
        | `Branch(n1,a1,xs1), `Branch(n2,a2,xs2) -> Name.equal n1 n2 && Datastruct.Art.equal a1 a2 && equal xs1 xs2
        | `Continue(xs1), `Continue(xs2) -> equal xs1 xs2
        | _, _ -> false

      let rec sanitize x =
        match x with
        | `Nil -> `Nil
        | `Ready(a) -> `Ready(Datastruct.Art.sanitize a)
        | `Data(x,xs) -> `Data(Data.sanitize x, sanitize xs)
        | `Art(n,a) -> `Art(Name.sanitize n, Datastruct.Art.sanitize a)
        | `Branch(n,a,xs) -> `Branch(Name.sanitize n, Datastruct.Art.sanitize a, sanitize xs)
        | `Continue(xs) -> `Continue(sanitize xs)
    
    end
    module Art = ArtLib.MakeArt(Name)(Data)
  end

  (* articulation module for data items *)
  module DArt = ArtLib.MakeArt(Name)(Data)
  (* articulation module for whole structure *)
  module SArt = Datastruct.Art


  (* TODO: add articulation points by 'max' and add `Continue's *)
  let art_struct_of_valued_list
    ?n:(name = Name.nondet())
    ?max:(max_elm = Params.max_unarticulated)
    ?min:(min_val = Params.min_value_branched)
    (value_of : 'a -> int)
    (data_of : 'a -> Data.t option)
    (input : 'a list)
    : SArt.t
  =
    let name_seed = ref name in
    let next_name () = 
      let ns, n = Name.fork !name_seed in
      name_seed := ns; n
    in
    let rec make_branch max_val data = 
      match data with
      | [] -> (`Nil,[])
      | x::xs ->
        let value = value_of x in
        if value > max_val then
          (* found a higher value, so this branch is over *)
          (`Nil,data)
        else if value = max_val || value < min_val then
          (* no reason to branch off *)
          let continue, leftover = make_branch(max_val)(xs) in
          match data_of x with
          | None -> (continue, leftover)
          | Some(x) ->
            (`Data(x, continue), leftover)
        else
          (* create a new branch *)
          let inner_branch, leftover = make_branch(max_val-1)(xs) in
          let outer_branch, leftover = make_branch(max_val)(leftover) in
          (`Branch(
              next_name(),
              SArt.cell (next_name()) (inner_branch),
              outer_branch
            ), leftover
          )
    in
    let main_branch = 
      (* first branch is put at proper level *)
      match input with
      | [] -> `Nil
      | x::xs -> 
        let first_value = value_of x in
        let rec build_up value data =
          let branch, leftover = make_branch(value)(data) in
          match leftover with
          | [] -> branch
          | more -> 
            let continue = build_up(value+1)(data) in
            `Branch(next_name(), SArt.cell (next_name()) branch, continue)
        in
        build_up first_value input
(* 
      (* first branch is put directly under root *)   
      let rec build_up input = 
      match input with
      | [] -> `Nil
      | x::xs -> 
        let first_value = value_of x in
        let branch, leftover = make_branch(first_value)(input) in
        match leftover with
        | [] -> branch
        | more -> 
          let continue = build_up more in
          `Branch(next_name(), SArt.cell (next_name()) branch, continue)
      in
      build_up input
 *)   
    in   
    SArt.cell name main_branch

  let more_funs = ()

end

module MakeSequence
  (ArtLib : ArtLibType)
  (Name   : NameType)
  (Data   : DatType)
  (Params : SParamsType)
= struct

  module Common 
(*     : StructType 
    with type ArtLib.lib_id = ArtLib.lib_id
    and type Name.t = Name.t
    and type Data.t = Data.t 
 *)  = struct
    include (MakeCommonStruct(ArtLib)(Name)(Data)(Params))
  end
  
  module ArtLib = Common.ArtLib
  module Name = Common.Name
  module Data = Common.Data

  module SeqData = Common.Datastruct.Data

  type t = Common.Datastruct.Art.t Common.art_struct

  (* articulation module for whole structure *)
  module SArt = Common.Datastruct.Art

  let art_list ?b:(branch = false) input = 
    let branching = (fun x -> ffs (Data.hash 0 x)) in
    let flat = (fun x -> 0) in
    Common.art_struct_of_valued_list 
      ~min:4 
      (if branch then branching else flat)
      (fun x -> Some(x))
      input


  let insert elm pos = ()

  let more_funs = ()

end
