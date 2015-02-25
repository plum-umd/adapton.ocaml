(** Struct: an experimental alternative to spreadtrees


TODO: determine what statistics need to be managed in this module

*)
open Adapton_core
open Primitives
open GrifolaType
module Types = AdaptonTypes
module Statistics = AdaptonStatistics

(* without this sig we can add arbitrary funs without modifying other code

  module type StructType = sig
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
(* no sig
    : StructType with type
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
  | `Branch of 'art art_struct * 'art art_struct
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
        | `Branch(xs,ys) -> "Branch("^(string xs)^","^(string xs)^")"
        | `Continue(xs) -> "Continue("^(string xs)^")"

      let rec hash seed x =
        match x with
        | `Nil -> Hashtbl.seeded_hash seed `Nil
        | `Ready(a) -> Datastruct.Art.hash seed a
        | `Data(x,xs) -> Data.hash (hash seed xs) x
        | `Art(n,a) -> Name.hash (Datastruct.Art.hash seed a) n
        | `Branch(xs,ys) -> hash (hash seed ys) xs
        | `Continue(xs) -> hash seed xs

      let rec equal xs ys =
        match xs, ys with
        | `Nil, `Nil -> true
        | `Ready(a1), `Ready(a2) -> Datastruct.Art.equal a1 a2
        | `Data(x1,xs1), `Data(x2,xs2) -> Data.equal x1 x2 && equal xs1 xs2
        | `Art(n1,a1), `Art(n2,a2) -> Name.equal n1 n2 && Datastruct.Art.equal a1 a2
        | `Branch(xs1,ys1), `Branch(xs2,ys2) -> equal xs1 xs2 && equal ys1 ys2
        | `Continue(xs1), `Continue(xs2) -> equal xs1 xs2
        | _, _ -> false

      let rec sanitize x =
        match x with
        | `Nil -> `Nil
        | `Ready(a) -> `Ready(Datastruct.Art.sanitize a)
        | `Data(x,xs) -> `Data(Data.sanitize x, sanitize xs)
        | `Art(n,a) -> `Art(Name.sanitize n, Datastruct.Art.sanitize a)
        | `Branch(xs, ys) -> `Branch(sanitize xs, sanitize ys)
        | `Continue(xs) -> `Continue(sanitize xs)
    
    end
    module Art = ArtLib.MakeArt(Name)(Data)
  end

  (* articulation module for data items *)
  module DArt = ArtLib.MakeArt(Name)(Data)
  (* articulation module for whole structure *)
  module SArt = Datastruct.Art

  let art_struct_of_valued_list
    ?n:(name = Name.nondet())                   (* name for this structure *)
    ?b:(branch_arts = 1)                        (* guarenteed articulation points per branch {0,1,2} *)
    ?max:(max_elm = Params.max_unarticulated)   (* maximum number of elements per art *)
    ?min:(min_val = Params.min_value_branched)  (* there is no branching with values under this *)
    (value_of : 'a -> int)                      (* function to access structure value *)
    (data_of : 'a -> Data.t option)             (* function to access data value *)
    (input : 'a list)                           (* input list to be structured *)
    : Datastruct.Art.t
  =
    (* set up some names *)
    let name_seed = ref name in
    let next_name () = 
      let ns, n = Name.fork !name_seed in
      name_seed := ns; n
    in
    (* recurse at branch point *)
    let rec do_branch level count input =
      (* enforce the max unarticulated elements through the branch *)
      let count = count / 2 in
      let first_branch, more =
        (* articulate at branch if requested *)
        if branch_arts > 0 then
          (* branches continue until the element value surpasses the level *)
          let inner, rest = do_linear (level-1) max_elm input in
          `Art(next_name(), SArt.cell (next_name()) inner), rest
        else
          do_linear (level-1) count input
      in
      let second_branch, rest =
        (* articulate at branch if requested *)
        if branch_arts > 1 then
          let inner, rest = do_linear (level-1) max_elm more in
          `Art(next_name(), SArt.cell (next_name()) inner), rest
        else
          do_linear level count more
      in
      `Branch(first_branch, second_branch), rest
    (* recurse at linear point *)
    and do_linear level count input =
      (* pack it all into a cell if it's time *)
      if count <= 0 then 
        let linear, rest = do_linear level max_elm input in
        `Art(next_name(), SArt.cell (next_name()) linear), rest
      else
      match input with
      | [] -> `Nil, []
      | x::xs ->
        (* next value is too high, so we're done, return the rest *)
        if value_of x > level then `Nil, input else
        (* next value is too low, so branch off *)
        if value_of x < level then do_branch level count input else
        match data_of x with
        (* structural marker, carry on *)
        | None -> do_linear level count xs
        (* this is our data, use it and continue *)
        | Some(x) -> 
          let continue, rest = do_linear level (count-1) xs in
          `Data(x, continue), rest
    in
    (* build 'backwards' to higher levels if nessecary *)
    let rec wrap_branch first_branch level input =
      let second_branch, rest = do_linear level max_elm input in
      let wrap = `Branch(first_branch, second_branch) in
      match rest with
      | [] -> wrap
      | more -> wrap_branch wrap (level+1) more
    in
    (* create struct *)
    let final_struct =
      match input with
      | [] -> `Nil
      | x::_ -> 
        let first_level = value_of x in
        let first_try, rest = do_linear first_level max_elm input in
        match rest with
        | [] -> first_try
        | more -> wrap_branch first_try (first_level+1) more
    in
    SArt.cell name final_struct

  let more_funs = ()

end

module MakeSequence
  (ArtLib : ArtLibType)
  (Name   : NameType)
  (Data   : DatType)
  (Params : SParamsType)
= struct

  module Common 
(*   no sig  
    : StructType 
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
    let purpose = Hashtbl.hash "structure" in
    let branching = (fun x -> ffs (Data.hash purpose x)) in
    let flat = (fun x -> 0) in
    Common.art_struct_of_valued_list 
      ~min:4 
      (if branch then branching else flat)
      (fun x -> Some(x))
      input


  let insert elm pos = failwith "insert - unimplemented"

  let more_funs = failwith "more_funs - unimplemented"

end
