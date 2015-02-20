(** Spread trees:

    Spread trees are a general-purpose data structure for (nominal,
    demand-driven) incremental computation.

    A spread tree can represent:
    -- a sequence (and in particular, an iterator), 
    -- a binary search tree, 
    -- a set, 
    -- a mapping, 
    -- a graph
    -- and more (?)

    Programmers use spreadtrees by constructing and computing with one
    of the above structures, and by inter-converting between them.
    The programmer rarely (if ever) sees the actual spreadtree
    structure, which is hidden by an API for one of the above
    structures.

    Internally, a "spreadtree" is a binary tree whose leaves consist
    of linked lists.  Both the tree's internal nodes and linked-list
    leaves hold data elements.  Incrementality is accomplished by
    interposing the recursive tree and list structures with Adapton
    names and articulation points (in particular, see
    GrifolaType.ArtType).

    == Tree and List structure:
    
    In Trees, we place data elements in two places: (1) at internal
    tree nodes and (2) at the lists in the leaves.  We use both places
    so as to trade off between tree structure and list structure in a
    flexible way.  

    ** This will be helpful for experiments, where we can measure the
    performance penalty of less tree-like data structures.

    By varying the size of leaf lists relative to the tree size, one
    trades off tree structure for list structure, either "spreading"
    the tree out linearly into a list, or gathering the leaf lists
    back into a bifurcated tree structure.  In either extreme, one
    either has a binary tree with empty leafs, or a single flat linked
    list.

    == Ropes versus SpreadTrees

    We use the term "rope" for a restricted structure (aka a "join
    list"), where there are three cases: Zero, One, Two, and where One
    cases carries one data element, and where the binary (Two) case
    carries no data elements, but just two sub-ropes.

    Since the Two case carries no data, it is helpful for writing
    certain computations, e.g., mergesort.  On the other hand, without
    data in the Two case, ropes cannot represent search trees (just
    sequences).

    == Use of incremental articulation points:

    Note: Nominal features are necessary to build and maintain trees
    efficiently.  Hence, these structures were designed with nominal
    incremental reuse in mind.  We expect that non-nominal thunks will
    not perform well incrementally, as compared with nominal thunks.

    We design "articulation points" (of laziness/incrementality) into
    the structures as a special recursive case, which can be present or
    absent with any frequency.  In one extreme, the structures have
    fine-grained articulated structure, and are maximally lazy and
    incremental.  In the other extreme, the structures have no
    articulated structure, and correspond exactly to immutable
    (purely-functional) structures that never change across time.

    Reasoning about articulation points isolates reasoning about
    laziness and nominal incrementality from reasoning about eager
    (and purely-functional) calculation steps.  Articulation cases are
    defined and used separately from the usual cases of the structure,
    which are defined in the usual (eager) fashion.
*)
open Adapton_core
open Primitives
open GrifolaType
module Types = AdaptonTypes
module Statistics = AdaptonStatistics

module type SpreadTreeType = sig
  module ArtLib : ArtLibType
  module Name : NameType
  module Data : DatType

  type 'art art_list = [ (* articulated list. *)
  | `Nil
  | `Cons of Data.t * 'art art_list
  | `Art of 'art
  | `Name of Name.t * 'art art_list
  ]

  type ('leaf, 'art) art_tree = [ (* articulated tree. *)
  | `Leaf of 'leaf
  | `Bin of ('leaf,'art) art_tree * Data.t * ('leaf,'art) art_tree
  | `Art of 'art
  | `Name of Name.t * ('leaf, 'art) art_tree
  ]

  type ('one, 'art) art_rope = [ (* articulated rope. *)
  | `Zero
  | `One of 'one
  | `Two of ('one,'art) art_rope * ('one,'art) art_rope
  | `Art of 'art
  | `Name of Name.t * ('one,'art) art_rope
  ]

  module rec List : sig
    module Data : DatType
    module Art  : ArtType
  end
    with type Data.t    = List.Art.t art_list
    and type Art.Data.t = List.Art.t art_list
    and type Art.Name.t = Name.t

  module rec Tree : sig
    module Data : DatType
    module Art  : ArtType
  end
    with type Data.t    = (List.Data.t, Tree.Art.t) art_tree
    and type Art.Data.t = (List.Data.t, Tree.Art.t) art_tree
    and type Art.Name.t = Name.t

  module rec Rope : sig
    module Data : DatType
    module Art  : ArtType
  end
    with type Data.t    = (Data.t, Rope.Art.t) art_rope
    and type Art.Data.t = (Data.t, Rope.Art.t) art_rope
    and type Art.Name.t = Name.t

end

(** Functor to make a Spread Tree module. *)
module MakeSpreadTree
  (ArtLib : ArtLibType)
  (Name   : NameType)
  (Data   : DatType)
  : SpreadTreeType with type ArtLib.lib_id = ArtLib.lib_id
                   and  type Name.t = Name.t
                   and  type Data.t = Data.t
=
struct
  module ArtLib = ArtLib
  module Name = Name
  module Data = Data

  type 'art art_list = [ (* articulated list. *)
  | `Nil
  | `Cons of Data.t * 'art art_list
  | `Art of 'art
  | `Name of Name.t * 'art art_list
  ]

  type ('leaf, 'art) art_tree = [ (* articulated tree. *)
  | `Leaf of 'leaf
  | `Bin of ('leaf,'art) art_tree * Data.t * ('leaf,'art) art_tree
  | `Art of 'art
  | `Name of Name.t * ('leaf,'art) art_tree
  ]

  type ('one, 'art) art_rope = [ (* articulated rope. *)
  | `Zero
  | `One of 'one
  | `Two of ('one,'art) art_rope * ('one,'art) art_rope
  | `Art of 'art
  | `Name of Name.t * ('one,'art) art_rope
  ]

  module rec List : sig
    module Data : DatType
    module Art  : ArtType
  end
    with type Data.t    = List.Art.t art_list
    and type Art.Data.t = List.Art.t art_list
    and type Art.Name.t = Name.t
  = struct
    module Data = struct
      type t = List.Art.t art_list

      let rec string x =
        ( match x with
        | `Nil -> "Nil"
        | `Cons(x,tl) -> ("Cons("^Data.string x)^","^(string tl)^")"
        | `Art a -> "Art("^List.Art.string a^")"
        | `Name(nm,xs) -> "Name("^(Name.string nm)^","^(string xs)^")"
        )

      let rec hash seed x =
        ( match x with
        | `Nil -> Hashtbl.seeded_hash seed `Nil
        | `Cons(x,tl) -> Data.hash (hash seed tl) x
        | `Art a -> List.Art.hash seed a
        | `Name(nm,xs) -> (Name.hash (hash seed xs) nm)
        )

      let rec equal xs ys =
        ( match xs, ys with
        | `Nil, `Nil -> true
        | `Cons(x,xs),`Cons(y,ys) -> Data.equal x y && equal xs ys
        | `Art a, `Art b -> List.Art.equal a b
        | `Name(nma,xs), `Name(nmb,ys) -> Name.equal nma nmb && equal xs ys
        | _, _ -> false
        )

      let rec sanitize x =
        ( match x with
        | `Nil -> `Nil
        | `Cons (x, tl) -> `Cons(Data.sanitize x, sanitize tl)
        | `Art a -> `Art (List.Art.sanitize a)
        | `Name(nm,xs) -> `Name(Name.sanitize nm, sanitize xs)
        )
    end
    module Art = ArtLib.MakeArt(Name)(Data)
  end

  module rec Tree : sig
    module Data : DatType
    module Art  : ArtType
  end
  with type Data.t    = (List.Data.t, Tree.Art.t) art_tree
  and type Art.Data.t = (List.Data.t, Tree.Art.t) art_tree
  and type Art.Name.t = Name.t
    =
  struct
    module Data = struct
      type t = (List.Data.t, Tree.Art.t) art_tree

      let rec string x =
        ( match x with
        | `Leaf xs -> "Leaf("^List.Data.string xs^")"
        | `Bin(l,x,r) -> "Bin("^(string l)^","^(Data.string x)^","^(string r)^")"
        | `Art a -> "Art("^Tree.Art.string a^")"
        | `Name(nm,t) -> "Name("^(Name.string nm)^","^(string t)^")"
        )

      let rec hash seed x =
        ( match x with
        | `Leaf xs -> List.Data.hash seed xs
        | `Bin(l,x,r) -> hash (Data.hash (hash seed l) x) r
        | `Art a -> Tree.Art.hash seed a
        | `Name(nm,x) -> (Name.hash (hash seed x) nm)
        )

      let rec equal x y =
        ( match x, y with
        | `Leaf xs, `Leaf ys -> List.Data.equal xs ys
        | `Bin(xl,x,xr), `Bin(yl,y,yr) -> Data.equal x y && equal xl yl && equal xr yr
        | `Art a, `Art b -> Tree.Art.equal a b
        | `Name(nma,x), `Name(nmb,y) -> Name.equal nma nmb && equal x y
        | _, _ -> false
        )

      let rec sanitize x =
        ( match x with
        | `Leaf x -> `Leaf (List.Data.sanitize x)
        | `Bin(l,x,r) -> `Bin(sanitize l, Data.sanitize x, sanitize r)
        | `Art a -> `Art(Tree.Art.sanitize a)
        | `Name(nm,x) -> `Name(Name.sanitize nm, sanitize x)
        )
    end
    module Art = ArtLib.MakeArt(Name)(Data)
  end
  
  module rec Rope : sig
    module Data : DatType
    module Art  : ArtType
  end
  with type Data.t    = (Data.t, Rope.Art.t) art_rope
  and type Art.Data.t = (Data.t, Rope.Art.t) art_rope
  and type Art.Name.t = Name.t
    =
  struct
    module Data = struct
      type t = (Data.t, Rope.Art.t) art_rope

      let rec string x =
        ( match x with
        | `Zero -> "Zero"
        | `One x -> "One("^Data.string x^")"
        | `Two(x,y) -> "Two("^string x^","^string y^")"
        | `Art a -> "Art("^Rope.Art.string a^")"
        | `Name(nm,t) -> "Name("^(Name.string nm)^","^(string t)^")"
        )

      let rec hash seed x =
        ( match x with
        | `Zero -> 0
        | `One x -> Data.hash seed x
        | `Two (x,y) -> hash (hash seed x) y
        | `Art a -> Rope.Art.hash seed a
        | `Name(nm,x) -> (Name.hash (hash seed x) nm)
        )

      let rec equal x y =
        ( match x, y with
        | `Zero, `Zero -> true
        | `One x, `One y -> Data.equal x y
        | `Two (x1,y1), `Two(x2,y2) -> (equal x1 x2) && (equal y1 y2)
        | `Art a, `Art b -> Rope.Art.equal a b
        | `Name(nma,x), `Name(nmb,y) -> Name.equal nma nmb && equal x y
        | _, _ -> false
        )

      let rec sanitize x =
        ( match x with
        | `Zero -> `Zero
        | `One x -> `One (Data.sanitize x)
        | `Two(x,y) -> `Two (sanitize x, sanitize y)
        | `Art a -> `Art(Rope.Art.sanitize a)
        | `Name(nm,x) -> `Name(Name.sanitize nm, sanitize x)
        )
    end
    module Art = ArtLib.MakeArt(Name)(Data)
  end
end

(* Sequences, based on SpreadTrees. *)
module MakeSeq
  ( St : SpreadTreeType )
= struct

  let default_granularity = 4

  module Name        = St.Name
  module AData       = St.ArtLib.MakeArt(Name)(St.Data)
  module ADataOption = St.ArtLib.MakeArt(Name)(Types.Option(St.Data))

  (* Abbreviations, for accessing mfn_* and force: *)
  module LArt = St.List.Art
  module TArt = St.Tree.Art
  module RArt = St.Rope.Art

  (* List of List Articulation *)
  module SToL = MakeSpreadTree(St.ArtLib)(Name)(St.List.Data)
  module LoLArt = SToL.List.Art


  (* ---------- an attempt at mutable list ----------- *)

  (* creates an articulated list *)
  let art_list
    ?g:(granularity=default_granularity)
    (input_list : St.Data.t list)
    : St.List.Data.t
  =
    let rec loop l =
      match l with
      | [] -> `Nil
      | x::xs ->
        if ffs (St.Data.hash 0 x) >= granularity then
          let nm1, nm2 = Name.fork (Name.nondet()) in
          `Cons(x, `Name(nm1, `Art (LArt.cell nm2 (loop xs))))
        else
          `Cons(x, (loop xs))
    in
    loop input_list

  (* returns a standard list *)
  let to_list (list : St.List.Data.t) : St.Data.t list = 
    let rec loop l =
      match l with
      | `Nil -> []
      | `Art(a) -> loop (LArt.force a)
      | `Name(_, xs) -> loop xs
      | `Cons(x, xs) -> x::(loop xs)
    in
    loop list

  (* inserts an element at the beginning of the list *)
  let list_cons
    ?g:(granularity=default_granularity)
    (h : St.Data.t)
    (tl : St.List.Data.t)
  =
    if ffs (St.Data.hash 0 h) >= granularity then
      let nm1, nm2 = Name.fork (Name.nondet()) in
      `Cons(h, `Name(nm1, `Art (LArt.cell nm2 tl)))
    else
      `Cons(h, tl)

  (* returns head and tail of list *)
  let list_snoc
    (list : St.List.Data.t)
    : (St.Data.t * St.List.Data.t) option
  =
    let rec loop l = 
      match l with
      | `Nil -> None
      | `Art(a) -> loop (LArt.force a)
      | `Name(_, xs) -> loop xs
      | `Cons(x, xs) -> Some(x, xs)
    in
    loop list


   (* --------------------------- *) 

  let mut_elms_of_list
    ( name : Name.t )
    ( list : 'a list )
    ( data_of : 'a -> St.Data.t )
    ( name_of : 'a -> Name.t ) 
    ( gran_level : int )
    : St.List.Art.t
  = 
    let rec loop list =
      match list with
      | [] -> `Nil
      | x :: xs ->
        if ffs (St.Data.hash 0 (data_of x)) >= gran_level then
          let nm1, nm2 = Name.fork (name_of x) in
          `Cons((data_of x), `Name(nm1, `Art (St.List.Art.cell nm2 (loop xs))))
        else
          `Cons((data_of x), (loop xs))
    in St.List.Art.cell name (loop list)

  let simple_full_string x =
    let rec loop = function
    | `Nil -> "Nil"
    | `Cons(x,xs) -> (St.Data.string x)^"; "^(loop xs)
    | `Art(a) -> "Art => "^(loop (LArt.force a))
    | `Name(_,xs) -> "Name; "^(loop xs)
    in
    loop (LArt.force x)


  let rec insert_elm list_art h nm_tl_opt =
    match nm_tl_opt with
    | Some (nm, tl_art) ->
      let list_art_content = St.List.Art.force list_art in
      St.List.Art.set list_art (`Cons(h, `Name(nm, `Art(tl_art)))) ;
      St.List.Art.set tl_art list_art_content
    | None ->
      let list_art_content = St.List.Art.force list_art in
      St.List.Art.set list_art (`Cons(h, list_art_content))      

  let rec delete_elm list_art =
    let (x,x_tl) =
      let rec loop list = 
        match list with
        | `Art art ->
          let elm, tl = loop (St.List.Art.force art) in
          elm, (`Art art)
            
        | `Name (nm, tl) ->
          let elm, tl = loop tl in
          elm, (`Name(nm, tl))
            
        | `Cons(x, tl) -> (x,tl)
        | `Nil -> failwith "delete_elm: Nil: No element to delete"
      in
      loop (St.List.Art.force list_art)
    in
    St.List.Art.set list_art x_tl ;
    (x,x_tl)

  let rec next_art x = match x with
    | `Nil -> None
    | `Cons(_, tl) -> next_art tl
    | `Name(_, rest) -> next_art rest
    | `Art a -> Some a

  let rec next_cons x =
    match x with
    | `Nil -> None
    | `Cons(x,xs) -> Some(x,xs)
    | `Art(a) -> next_cons(LArt.force a)
    | `Name(_, xs) -> next_cons xs

  let rec ith_art list count = 
    ( match count with
    | x when x <= 0 -> list
    | _ -> match list with
      | `Nil -> `Nil
      | `Cons(x, xs) -> ith_art xs (count-1)
      | `Name(_, xs) -> ith_art xs count
      | `Art a -> ith_art (St.List.Art.force a) count
    )

  let rec take list count =
    let dec = function
      | Some count -> Some (count-1)
      | None -> None
    in
    ( match count with
    | Some count when (count <= 0) -> []
    | _ ->       
      match list with
      | `Nil -> []
      | `Cons(x, xs) -> x :: (take xs (dec count))
      | `Name(_, xs) -> take xs count
      | `Art a -> take (St.List.Art.force a) count
    )

  let rec list_is_empty ( list : St.List.Data.t) : bool =
    ( match list with
    | `Nil -> true
    | `Cons(_,_) -> false
    | `Art a -> list_is_empty ( LArt.force a )
    | `Name (_,x) -> list_is_empty x
    )

  let list_append = 
    let mfn = LArt.mk_mfn (St.Name.gensym "list_append")
      (module Types.Tuple2(St.List.Data)(St.List.Data))
      (fun r (xs, ys) ->
        let list_append xs ys = r.LArt.mfn_data (xs,ys) in
        ( match xs with
        | `Nil -> ys
        | `Cons(x,tl) -> `Cons(x, list_append tl ys)
        | `Art a -> list_append (LArt.force a) ys
        | `Name(nm,xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art (r.LArt.mfn_nart nm2 (xs, ys)))
        ))
    in
    fun xs ys -> mfn.LArt.mfn_data (xs, ys)

  let list_of_tree : St.Tree.Data.t -> St.List.Data.t -> St.List.Data.t =
    let mfn = LArt.mk_mfn (St.Name.gensym "list_of_tree")      
      (module Types.Tuple2(St.Tree.Data)(St.List.Data))
      (fun r (tree, rest) ->
        let list_of_tree tree list = r.LArt.mfn_data (tree, list) in
        ( match tree with
        | `Leaf xs           -> list_append xs rest
        | `Bin(left,x,right) -> list_of_tree left (`Cons(x, list_of_tree right rest))
        | `Art art           -> list_of_tree (TArt.force art) rest
        | `Name(nm,tree)     -> let nm1,nm2 = Name.fork nm in
                                `Name(nm1, `Art(r.LArt.mfn_nart nm2 (tree, rest)))
        ))
    in
    fun tree list -> mfn.LArt.mfn_data (tree, list)

  let tree_of_list_rec : int -> int -> St.Tree.Data.t -> St.List.Data.t -> St.Tree.Data.t * St.List.Data.t =
    let module P = St.ArtLib.MakeArtTuple2(Name)(St.Tree.Art)(St.List.Art) in
    let tree_of_list_rec =
      let mfn = P.Art.mk_mfn (St.Name.gensym "tree_of_list_rec")
        (module Types.Tuple4(Types.Int)(Types.Int)(St.Tree.Data)(St.List.Data))
        (fun r (parent_lev, tree_lev, tree, list) ->
          let tree_of_list_rec pl tl t l = r.P.Art.mfn_data (pl,tl,t,l) in
          ( match list with
          | `Nil -> tree, `Nil
          | `Cons (hd, tl) ->
            let ffs = Primitives.ffs in
            let hd_lev = ffs (St.Data.hash 0 hd) in
            if tree_lev <= hd_lev && hd_lev <= parent_lev then
              let right, rest = tree_of_list_rec hd_lev (-1) (`Leaf `Nil) tl in
              let tree = `Bin(tree, hd, right) in
              tree_of_list_rec parent_lev hd_lev tree rest
            else
              tree, list
          | `Art art -> tree_of_list_rec parent_lev tree_lev tree (St.List.Art.force art)
          | `Name(nm, list) -> 
            let nm1, nm  = Name.fork nm in
            let nm2, nm3 = Name.fork nm in
            let tree_rest = r.P.Art.mfn_nart nm1 (parent_lev,tree_lev,tree,list) in
            let _, rest = P.Art.force tree_rest in
            let tree = P.fst nm2 tree_rest in
            (`Name(nm3, `Art tree), rest)
          ))
      in
      fun pl tl t l -> mfn.P.Art.mfn_data (pl, tl, t, l)
    in
    tree_of_list_rec

  let tree_of_list : St.List.Data.t -> St.Tree.Data.t =
    fun list -> 
      ( match (tree_of_list_rec max_int (-1) (`Leaf `Nil) list) with 
      | tree, `Nil -> tree
      | _ -> failwith "tree_of_list: impossible"
      )

  let rope_of_list_rec : int -> int -> St.Rope.Data.t -> St.List.Data.t -> St.Rope.Data.t * St.List.Data.t =
    let module P = St.ArtLib.MakeArtTuple2(Name)(St.Rope.Art)(St.List.Art) in
    let rope_of_list_rec =
      let mfn = P.Art.mk_mfn (St.Name.gensym "rope_of_list_rec")
        (module Types.Tuple4(Types.Int)(Types.Int)(St.Rope.Data)(St.List.Data))
        (fun r (parent_lev, rope_lev, rope, list) ->
          let rope_of_list_rec pl tl t l = r.P.Art.mfn_data (pl,tl,t,l) in
          ( match list with
          | `Nil -> rope, `Nil
          | `Cons (hd, tl) ->
            let ffs = Primitives.ffs in
            let hd_lev = ffs (St.Data.hash 0 hd) in
            if rope_lev <= hd_lev && hd_lev <= parent_lev then (
              let right, rest = rope_of_list_rec hd_lev (-1) (`One hd) tl in
              let rope = `Two(rope, right) in
              rope_of_list_rec parent_lev hd_lev rope rest
            ) 
            else (
              rope, list
            )
          | `Art art -> rope_of_list_rec parent_lev rope_lev rope (St.List.Art.force art)
          | `Name(nm, list) -> 
            let nm1, nm  = Name.fork nm in
            let nm2, nm3 = Name.fork nm in
            let rope_rest = r.P.Art.mfn_nart nm1 (parent_lev,rope_lev,rope,list) in
            let _,   rest = P.Art.force rope_rest in
            let rope = P.fst nm2 rope_rest in (* TODO: Perf opt: Avoid making extra thunk here. *)
            (`Name(nm3, `Art rope), rest) (* BUGFIX: Do "primitive recursion": Do not introduce a `Name, or `Art around rest. *)
          ))
      in
      fun pl tl t l -> mfn.P.Art.mfn_data (pl, tl, t, l)
    in
    rope_of_list_rec

  let rope_of_list : St.List.Data.t -> St.Rope.Data.t =
    fun list -> 
      let rope, rest = 
        rope_of_list_rec max_int (-1) (`Zero) list
      in
      (* assert (list_is_empty rest) ; *)
      rope

  let list_of_rope : St.Rope.Data.t -> St.List.Data.t -> St.List.Data.t =
    let mfn = LArt.mk_mfn (St.Name.gensym "list_of_rope")      
      (module Types.Tuple2(St.Rope.Data)(St.List.Data))
      (fun r (rope, rest) ->
        let list_of_rope rope list = r.LArt.mfn_data (rope, list) in
        ( match rope with
        | `Zero          -> rest
        | `One x         -> `Cons(x, rest)
        | `Two(x,y)      -> list_of_rope x (list_of_rope y rest)
        | `Art art       -> list_of_rope (RArt.force art) rest
        | `Name(nm,rope) -> let nm1,nm2 = Name.fork nm in
                            `Name(nm1, `Art(r.LArt.mfn_nart nm2 (rope, rest)))
        ))
    in
    fun rope list -> mfn.LArt.mfn_data (rope, list)

  let rope_length : St.Rope.Data.t -> int =
    let module Len = St.ArtLib.MakeArt(Name)(Types.Int) in
    let mfn = Len.mk_mfn (St.Name.gensym "rope_length")
      (module St.Rope.Data)
      (fun r rope ->
        let len rope = r.Len.mfn_data rope in
        let memo_len n rope = r.Len.mfn_nart n rope in
        match rope with
        | `Zero -> 0
        | `One(x) -> 1
        | `Two(r1,r2) -> (len r1) + (len r2)
        | `Art(a) -> len (RArt.force a)
        | `Name(nm, r) -> Len.force (memo_len nm r)
      )
    in
    fun rope -> mfn.Len.mfn_data rope

  (* non-memoised indexed lookup of a rope, using memoized rope_length for speed *)
  let rope_nth rope n : St.Data.t option = 
    if rope_length rope <= n then None else
    (* main work after initial checks *)
    let rec rope_nth rope n =
      match rope with
      | `Zero -> failwith "rope_nth: bad length reporting"
      | `One(x) -> if n = 0 then Some(x) else failwith "rope_nth: bad length reporting"
      | `Two(r1,r2) -> 
        let r1l = rope_length r1 in
        if r1l > n then
          rope_nth r1 n
        else
          rope_nth r2 (n-r1l)
      | `Art(a) -> rope_nth (RArt.force a) n
      | `Name(nm, r) -> rope_nth r n
    in
    rope_nth rope n
      


(*
  let rec tree_append ( left : St.Tree.Data.t ) ( right : St.Tree.Data.t ) =
    ( match left, right with
    | `Art a, right -> tree_append ( St.Tree.Art.force a ) right
    | left, `Art a  -> tree_append left ( St.Tree.Art.force a )

    | `Leaf `Nil, _ -> right
    | _, `Leaf `Nil -> left

    | `Leaf xs, `Leaf `Cons(y,ys) -> `Bin (`Leaf xs, y, `Leaf ys)

    | `Leaf xs, `Leaf `Art a -> tree_append (`Leaf xs) (`Leaf (St.List.Art.force a))

    | `Bin(xleft,x,xright), `Leaf ys ->
      failwith "TODO"

    | `Leaf xs, `Bin(yleft,y,yright) ->
      failwith "TODO"

    | `Bin(xleft,x,xright), `Bin(yleft,y,yright) ->
      failwith "TODO"

    | `Name _, _ -> failwith "Missing code here."
    | _, `Name _ -> failwith "Missing code here."
    )
*)

  let tree_append left right = failwith "TODO"

  let rec tree_pop_front (tree : St.Tree.Data.t) : (St.Tree.Data.t * St.Data.t) option =
    failwith "TODO"

  let list_reverse : St.List.Data.t -> St.List.Data.t -> St.List.Data.t =
    let mfn = LArt.mk_mfn (St.Name.gensym "list_reverse")
      (module Types.Tuple2(St.List.Data)(St.List.Data))
      (fun r (list, rev) ->
        let list_reverse list rev = r.LArt.mfn_data (list,rev) in
        ( match list with
        | `Nil -> rev
        | `Cons(x, xs) -> list_reverse xs (`Cons(x, rev))
        | `Art art -> list_reverse (LArt.force art) rev
        | `Name (nm, xs) -> let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art (r.LArt.mfn_nart nm2 (xs, rev)))
        ))
    in
    fun list rev -> mfn.LArt.mfn_data (list, rev)

  let rec tree_reverse ( tree : St.Tree.Data.t ) =
    let mfn = TArt.mk_mfn (St.Name.gensym "tree_reverse")
      (module St.Tree.Data)
      (fun r tree -> let tree_reverse = r.TArt.mfn_data in
        ( match tree with
        | `Leaf xs -> `Leaf ( list_reverse xs `Nil )
        | `Bin(left,x,right) -> `Bin( tree_reverse right, x, tree_reverse left)
        | `Art art -> tree_reverse (TArt.force art)
        | `Name(nm, t) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.TArt.mfn_nart nm2 t))
        ))
    in
    fun tree -> mfn.TArt.mfn_data tree

  let rec rope_reverse ( rope : St.Rope.Data.t ) =
    let mfn = RArt.mk_mfn (St.Name.gensym "rope_reverse")
      (module St.Rope.Data)
      (fun r rope -> let rope_reverse = r.RArt.mfn_data in
        ( match rope with
        | `Zero -> `Zero
        | `One x -> `One x
        | `Two(x,y) -> `Two(rope_reverse y, rope_reverse x)
        | `Art art -> rope_reverse (RArt.force art)
        | `Name (nm,rope) -> 
          let nm1,nm2 = Name.fork nm in
          `Name(nm1, `Art(r.RArt.mfn_nart nm2 rope))
        ))
    in
    fun rope -> mfn.RArt.mfn_data rope

  (* packs each element as a single item list inside a list of lists *)
  let list_to_singletons
      : St.List.Data.t -> SToL.List.Data.t =
    let fnn = St.Name.gensym "list_to_singletons" in
    let mfn = LoLArt.mk_mfn fnn
      (module Types.Tuple2(Types.Option(Name))(St.List.Data))
      (fun r (nm_opt, list) ->
        let single xs = r.LoLArt.mfn_data (nm_opt, xs) in
        let single_n nm xs = r.LoLArt.mfn_data (Some(nm), xs) in
        let single_cons (x, xs) =
          match nm_opt with
          | None -> `Cons(`Cons(x, `Nil), single xs)
          | Some(nms) ->
            let nm1, nms = Name.fork nms in
            let nm2, nms = Name.fork nms in
            let nm3, nm4 = Name.fork nms in
            (* TODO: the creation of the inner list should be done with a list creator to properly articulate the list *)
            `Name(nm1, `Cons(
              `Name(nm2,(`Cons(x, `Art(LArt.cell nm3 `Nil)))),
              `Art(r.LoLArt.mfn_nart nm4 (None, xs))
            ))
        in
        match list with
        | `Nil -> `Nil
        | `Cons(x,xs) -> single_cons (x, xs)
        | `Art(a) -> single (LArt.force a)
        | `Name(nm, xs) -> single_n nm xs
      )
    in
    fun list -> mfn.LoLArt.mfn_data (None, list)

  (* TODO: bring out some internal funs *)
  (* repeatedly applies the contraction function probabilistically until we reach a single value *)
  let list_contract
      (f_nm : St.Name.t)
      (contract_f : SToL.Data.t -> SToL.Data.t -> SToL.Data.t)
      : St.List.Data.t -> St.List.Data.t =
    let fnns = St.Name.pair (St.Name.gensym "list_contract") f_nm in
    let fnn1, fnn2 = Name.fork fnns in
    let singletons = list_to_singletons in
    (* Probabilistically apply contraction function *)
    let mfn_contr = LoLArt.mk_mfn fnn1
      (module Types.Tuple3(Types.Int)(Types.Option(Name))(SToL.List.Data))
      (fun r (seed, nm_opt, lol) ->
        (* common recursion, used with `Art *)
        let contr xs = r.LoLArt.mfn_data (seed, nm_opt, xs) in
        (* recursion with a new name, used with `Name *)
        let contr_n nm xs = r.LoLArt.mfn_data (seed, nm, xs) in
        (* memoised recursion with data, used to wrap data with names *)
        let contr_cons (x, xs) =
          match nm_opt with
          | None -> `Cons(x, contr xs)
          | Some(nm) ->
            let nm1, nm2 = Name.fork nm in
            `Name(nm1, `Cons(x, `Art(r.LoLArt.mfn_nart nm2 (seed, None, xs))))
        in
        match lol with
        | `Nil -> `Nil
        | `Cons(x,xs) ->
          if ((SToL.Data.hash seed x) mod 2) = 0 then
            contr_cons(x, xs)
          else 
            (match xs with
            | `Nil -> contr_cons(x, `Nil)
            | `Cons(y,ys) -> contr_cons((contract_f x y), ys)
            | `Art(a) -> contr (`Cons(x, LoLArt.force a))
            | `Name(nm, ys) -> contr_n (Some(nm)) (`Cons(x,ys))
            )
        | `Art(a) -> contr (LoLArt.force a)
        | `Name(nm, xs) -> contr_n (Some(nm)) xs
      )
    in
    (* 
      reduce by applying contract_f to make progress,
      then calling mfn_contr to handle the rest
    *)
    let mfn_reduce = LArt.mk_mfn fnn2
      (module Types.Tuple2(Types.Seeds)(SToL.List.Data))
      (fun r (sd,lol) ->
        let rnd, sd = Types.Seeds.pop sd in
        let contr xs = mfn_contr.LoLArt.mfn_data (rnd, None, xs) in
        let reduce list = r.LArt.mfn_data (sd, list) in
        let reduce_n nm list = r.LArt.mfn_nart nm (sd, list) in
        (*
          Here we are using recursion as a loop. Later Names
          are maintained by the contract function, and the
          first items are contracted once here. This allows
          us to take advantage of available names for
          articulated recursion
        *)
        match lol with
        | `Nil -> `Nil
        | `Cons(x,xs) ->
          (match xs with
          | `Nil -> x
          | `Cons(y,ys) -> reduce (`Cons(contract_f x y, contr ys))
          | `Art(a) -> reduce (`Cons(x, LoLArt.force a))
          | `Name(nm, ys) -> LArt.force (reduce_n nm (`Cons(x,ys)))
          )
        | `Art(a) -> reduce (LoLArt.force a)
        | `Name(nm, xs) -> LArt.force (reduce_n nm xs)
      )
    in
    fun list -> 
      let sd = Types.Seeds.make() in
      let lol = singletons list in
      mfn_reduce.LArt.mfn_data (sd, lol)

  let list_reduce 
      (op_nm : St.Name.t) 
      (op : St.Data.t -> St.Data.t -> St.Data.t) 
      : St.List.Data.t -> St.Data.t option =
    let fnn = St.Name.pair (St.Name.gensym "list_reduce") op_nm in
    let mfn = ADataOption.mk_mfn fnn 
      (module St.List.Data)
      ( fun r list ->
        let list_reduce = r.ADataOption.mfn_data in
        ( match list with
        | `Nil -> None
        | `Cons(x,tl) ->
          ( match list_reduce tl with
          | None -> Some x
          | Some y -> Some (op x y))
        
        | `Art art -> list_reduce (St.List.Art.force art)
        | `Name (nm, xs) -> 
          (* TODO: Use iterative (multi-round) contraction here.. *)
          ADataOption.force (r.ADataOption.mfn_nart nm xs)
        ))          
    in
    fun list -> mfn.ADataOption.mfn_data list

  let rec tree_reduce
      ( op_nm : St.Name.t )
      ( op : St.Data.t -> St.Data.t -> St.Data.t )
      : St.Tree.Data.t -> St.Data.t option =
    let fnn = St.Name.pair (St.Name.gensym "tree_reduce") op_nm in
    let list_reduce = list_reduce op_nm op in
    let mfn = ADataOption.mk_mfn fnn 
      (module St.Tree.Data)
      (fun r tree ->
        let tree_reduce = r.ADataOption.mfn_data in
        ( match tree with
        | `Leaf xs -> list_reduce xs
        | `Bin(left,x,right) ->
          let x_right =
            match tree_reduce right with
            | Some y -> Some (op x y)
            | None -> Some x
          in
          ( match tree_reduce left, x_right with
          | Some l, Some r -> Some (op l r)
          | Some l, None   -> Some l
          | None,   Some r -> Some r
          | None,   None   -> None
          )
        
        | `Art art -> tree_reduce (St.Tree.Art.force art)
        | `Name (nm, tree) -> 
          ADataOption.force (r.ADataOption.mfn_nart nm tree)
        ))
    in
    fun tree -> mfn.ADataOption.mfn_data tree

  let rec rope_reduce
      ( op_nm : St.Name.t )
      ( op : St.Data.t -> St.Data.t -> St.Data.t )
      : St.Rope.Data.t -> St.Data.t option =
    let fnn = St.Name.pair (St.Name.gensym "rope_reduce") op_nm in
    let mfn = ADataOption.mk_mfn fnn 
      (module St.Rope.Data)
      (fun r rope ->
        let rope_reduce = r.ADataOption.mfn_data in
        ( match rope with
        | `Zero  -> None
        | `One x -> Some x
        | `Two(left,right) ->
          ( match rope_reduce left, rope_reduce right with
          | Some l, Some r -> Some (op l r)
          | Some l, None   -> Some l
          | None,   Some r -> Some r
          | None,   None   -> None
          )
        
        | `Art art -> rope_reduce (St.Rope.Art.force art)        
        | `Name (nm1, `Name(nm2, rope)) -> 
          let nm = if Name.height nm1 > Name.height nm2 then nm1 else nm2 in
          rope_reduce (`Name(nm, rope))
        | `Name (nm, rope) -> 
          ADataOption.force (r.ADataOption.mfn_nart nm rope)
        ))
    in
    fun rope -> mfn.ADataOption.mfn_data rope

  (* finds the median of a rope in current order, sort first to find true median *)
  let rope_median rope : St.Data.t option =
    let len = rope_length rope in
    if len = 0 then None else
    let mid = len/2 in
    rope_nth rope mid

  let list_merge
      (compare_nm : St.Name.t)
      (compare : St.Data.t -> St.Data.t -> int)
      : St.List.Data.t -> St.List.Data.t -> St.List.Data.t =
    let fnn = St.Name.pair (St.Name.gensym "list_merge") compare_nm in
    let mfn = LArt.mk_mfn fnn
      (module Types.Tuple4
        (Types.Option(Name)) (Types.Option(Name))
        (St.List.Data) (St.List.Data)
      )
      (fun r (nm_opt1,nm_opt2,list1,list2) ->
        let merge xs ys = r.LArt.mfn_data (nm_opt1,nm_opt2,xs,ys) in
        let merge_nms nm1 nm2 xs ys = r.LArt.mfn_data (nm1,nm2,xs,ys) in
        let merge_cons1 x l1 l2 = 
          match nm_opt1 with
          | None -> `Cons(x, merge l1 l2)
          | Some(nms) -> 
            let nm1,nm2 = Name.fork nms in
            `Name(nm1, `Cons(x, `Art(r.LArt.mfn_nart nm2 (None, nm_opt2, l1, l2))))
        in
        let merge_cons2 y l1 l2 = 
          match nm_opt2 with
          | None -> `Cons(y, merge l1 l2)
          | Some(nms) -> 
            let nm1,nm2 = Name.fork nms in
            `Name(nm1, `Cons(y, `Art(r.LArt.mfn_nart nm2 (nm_opt1, None, l1, l2))))
        in
        match list1, list2 with
        | `Nil, _ -> list2
        | _, `Nil -> list1
        | `Art(a1), _ -> merge (LArt.force a1) list2
        | _, `Art(a2) -> merge list1 (LArt.force a2)
        | `Name(nm1, xs1), _ -> merge_nms (Some(nm1)) nm_opt2 xs1 list2
        | _, `Name(nm2, xs2) -> merge_nms nm_opt1 (Some(nm2)) list1 xs2
        | `Cons(x,xs), `Cons(y,ys) -> 
          incr Statistics.Counts.unit_cost ;
          if compare x y <= 0 then 
            merge_cons1 x xs list2
          else
            merge_cons2 y list1 ys
      )
    in
    fun xs ys -> mfn.LArt.mfn_data (None,None,xs,ys)

  (* prep for quicksort - untested *)
  let list_split_on_pivot
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int)
      : St.List.Data.t -> St.Data.t -> (St.List.Data.t * St.List.Data.t) =
    let module M = St.ArtLib.MakeArt(Name)(Types.Tuple2(St.List.Data)(St.List.Data)) in
    let fnn = St.Name.pair (St.Name.gensym "list_split_on_pivot") compare_nm in
    let mfn = M.mk_mfn fnn
      (module Types.Tuple4(St.List.Data)(St.Data)(St.List.Data)(St.List.Data))
      (fun r (list, c, l1, l2) ->
        let split_list list c l1 l2 = r.M.mfn_data (list, c, l1, l2) in
        ( match list with
        | `Nil -> (l1,l2)
        | `Cons(x,xs) -> 
          if compare x c <= 0 then
            split_list xs c (`Cons(x,l1)) l2
          else
            split_list xs c l1 (`Cons(x,l2))
        | `Art(a) -> split_list (LArt.force a) c l1 l2
        | `Name(nms, xs) -> 
          let nm1, nms = Name.fork nms in
          let nm2, nms = Name.fork nms in
          let nm3, nm4 = Name.fork nms in
          split_list xs c
            (`Name(nm1, `Art(LArt.cell nm2 l1)))
            (`Name(nm3, `Art(LArt.cell nm4 l2)))
      ))
    in
    fun list c -> mfn.M.mfn_data (list, c, `Nil, `Nil)

  let list_mergesort
      ( nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.List.Data.t -> St.List.Data.t = 
    let fnn = St.Name.pair (St.Name.gensym "list_mergesort") nm in
    let merge = list_merge fnn compare in
    let contract = list_contract fnn merge in
    fun list -> contract list

  let rope_mergesort
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.Rope.Data.t -> St.List.Data.t =
    let fnn = St.Name.pair (St.Name.gensym "rope_mergesort") compare_nm in
    let merge = list_merge compare_nm compare in
    let mfn = St.List.Art.mk_mfn fnn
      (module St.Rope.Data)
      (fun r rope ->
        let rope_mergesort = r.LArt.mfn_data in
        ( match rope with
        | `Zero -> `Nil
        | `One x -> `Cons(x, `Nil)
        | `Two(x, y) ->
          let x_sorted = rope_mergesort x in
          let y_sorted = rope_mergesort y in
          merge x_sorted y_sorted

        | `Art art -> rope_mergesort (RArt.force art)
        | `Name (nm, rope) ->
          let nm1,nm2 = Name.fork nm in
          `Name(nm1, `Art (r.LArt.mfn_nart nm2 rope))
        ))
    in
    fun rope -> mfn.LArt.mfn_data rope

  let list_to_rope_mergesort 
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.List.Data.t -> St.List.Data.t =
    let sort = rope_mergesort compare_nm compare in
    fun list ->
      let rope = rope_of_list list in 
      sort rope

end

(* Makes a key-Value mapping, based on SpreadTrees.
   The mapping is represented as a tree of key-value-sequence pairs.
   Keys are ordered by a comparison function.
   The tree is a binary search tree according to this comparison function.
   Value sequences are stored in an unordered fashion.
*)
module MakeKvMap
  ( ArtLib : ArtLibType )
  ( Name : NameType )
  ( KeyData : ( sig
    include DatType
    val compare : t -> t -> int
  end ))
  ( ValSt : SpreadTreeType
    with type ArtLib.lib_id = ArtLib.lib_id
    and  type Name.t        = Name.t
  ) =

struct
  module KeyData   = KeyData
  module ValSt     = ValSt
(*   module Kv        = Types.Tuple2 ( KeyData )( ValSt.List.Data ) *)
(*  let get_key = fst *)
(*  let empty_kv k = (k, `Nil) *)    

  module Kv = KeyData (* TEMP *)
  let get_key x = x
  let empty_kv k = k

  module AKvOption = ArtLib.MakeArt( Name )( Types.Option( Kv ) )
  module KvSt      = MakeSpreadTree ( ArtLib )( Name )( Kv )
  module KvSeq     = MakeSeq ( KvSt )
  module ValSeq    = MakeSeq ( ValSt )
  module ABool     = ArtLib.MakeArt( Name )( Types.Bool )
  module TArt      = KvSt.Tree.Art
  module LArt      = KvSt.List.Art

  let rec is_bst : KeyData.t * KeyData.t  -> KvSt.Tree.Data.t -> bool =
    let mfn = ABool.mk_mfn (Name.gensym "is_bst")
      (module (Types.Tuple3(KeyData)(KeyData)(KvSt.Tree.Data)))
      (fun r (lo,hi,tree) ->
        let is_bst (lo,hi) tree = r.ABool.mfn_data (lo,hi,tree) in        
        ( match tree with
        | `Leaf `Nil -> true
        | `Leaf `Art art -> is_bst (lo,hi) (`Leaf (KvSt.List.Art.force art))
        | `Leaf `Name (_,rest) -> is_bst (lo,hi) (`Leaf rest)
        | `Leaf `Cons(kv, rest) ->
          KeyData.compare lo (get_key kv) <= 0
          && KeyData.compare (get_key kv) hi <= 0
          && KvSeq.list_is_empty rest
        | `Bin(left,kv,right) -> let x = get_key kv in
          ( KeyData.compare lo x <= 0
            && KeyData.compare x hi <= 0
            && is_bst (lo, x) left
            && is_bst (x, hi) right )
            
        | `Art art -> is_bst (lo,hi) (KvSt.Tree.Art.force art)
        | `Name (nm,tree) -> ABool.force (r.ABool.mfn_nart nm (lo,hi,tree))
        ))
    in
    fun (lo,hi) tree -> mfn.ABool.mfn_data (lo,hi,tree)

  let rec list_find
      ( list : KvSt.List.Data.t )
      ( target : KeyData.t ) : KvSt.Data.t option =
    ( match list with
    | `Nil -> None
    | `Cons(kv, tl) ->
      if KeyData.compare (get_key kv) target = 0 then Some kv
      else list_find tl target
    
    | `Art art -> list_find (KvSt.List.Art.force art) target
    | `Name (_,list) -> list_find list target
    )

  let rec tree_find
      ( tree : KvSt.Tree.Data.t )
      ( target : KeyData.t ) : KvSt.Data.t option =
    ( match tree with
    | `Leaf xs -> list_find xs target
    | `Bin(left,kv,right) ->
      let ord = KeyData.compare target (get_key kv) in
      if ord < 0 then      tree_find left target
      else if ord > 0 then tree_find right target
      else if ord = 0 then Some kv
      else failwith "impossible"
    
    | `Art art -> tree_find (KvSt.Tree.Art.force art) target
    | `Name (_,tree) -> tree_find tree target
    )

  let rec list_remove 
      : KvSt.List.Data.t -> KeyData.t -> (KvSt.Data.t option) * KvSt.List.Data.t = 
    let module M = ArtLib.MakeArtTuple2(Name)(AKvOption)(KvSt.List.Art) in
    let mfn = M.Art.mk_mfn (Name.gensym "list_remove")
      (module (Types.Tuple2(KvSt.List.Data)(KeyData)))
      (fun r (list, target) ->
        let list_remove list target = r.M.Art.mfn_data (list, target) in
        ( match list with
        | `Nil -> ( None, `Nil )
        | `Cons(kv, rest) ->
          if KeyData.compare (get_key kv) target = 0 then
            ( Some kv, rest )
          else
            let res, rem = list_remove rest target in
            (res, `Cons(kv, rem))
    
        | `Art art -> list_remove (KvSt.List.Art.force art) target
        | `Name (nm, list) -> 
          let nm1,nm  = Name.fork nm in
          let nm2,nm3 = Name.fork nm in
          let elm_rem = r.M.Art.mfn_nart nm1 (list, target) in
          let elm,rem = M.split nm2 elm_rem in
          M.Art1.force elm, `Name(nm3, `Art rem)
        ))
    in
    fun list target -> mfn.M.Art.mfn_data (list, target)

  let rec tree_remove
      : KvSt.Tree.Data.t -> KeyData.t -> (KvSt.Data.t option) * KvSt.Tree.Data.t =
    let module M = ArtLib.MakeArtTuple2(Name)(AKvOption)(KvSt.Tree.Art) in
    let mfn = M.Art.mk_mfn (Name.gensym "tree_remove")
      (module (Types.Tuple2(KvSt.Tree.Data)(KeyData)))
      (fun r (tree, target) ->
        let tree_remove tree target = r.M.Art.mfn_data (tree, target) in
        ( match tree with
        | `Leaf xs ->
          let res, ys = list_remove xs target in
          (res, `Leaf ys)
        | `Bin(left, kv, right) ->
          let ord = KeyData.compare target (get_key kv) in
          if ord < 0 then      tree_remove left target
          else if ord > 0 then tree_remove right target
          else if ord = 0 then (Some kv, `Bin(left, empty_kv (get_key kv), right))
          else failwith "impossible"
            
        | `Art art -> tree_remove (KvSt.Tree.Art.force art) target
        | `Name (nm, tree) -> 
          let nm1, nm  = Name.fork nm in
          let nm2, nm3 = Name.fork nm in
          let elm_rem = r.M.Art.mfn_nart nm1 (tree, target) in
          let elm,rem = M.split nm2 elm_rem in
          M.Art1.force elm, `Name(nm3, `Art rem)
        ))
    in
    fun tree target -> mfn.M.Art.mfn_data (tree, target)          

  let tree_height 
      : KvSt.Tree.Data.t -> int =
    let module M = ArtLib.MakeArt(Name)(Types.Int) in
    let mfn = M.mk_mfn (Name.gensym "tree_height")
      (module KvSt.Tree.Data)
      (fun r tree ->
        let tree_height tree = r.M.mfn_data tree in
        ( match tree with
        | `Leaf xs -> (-1)
        | `Bin(left,x,right) ->
          let hleft = tree_height left in
          let hright = tree_height right in
          1 + (if hleft > hright then hleft else hright)
            
        | `Art art -> tree_height (KvSt.Tree.Art.force art)
        | `Name (nm, tree) -> M.force (r.M.mfn_nart nm tree)
        ))
    in
    fun tree -> mfn.M.mfn_data tree

  let rec tree_height_diff ( tree : KvSt.Tree.Data.t ) : int =
    ( match tree with
    | `Leaf _ -> 0
    | `Bin(left,x,right) -> (tree_height left) - (tree_height right)
    | `Art art -> tree_height_diff (KvSt.Tree.Art.force art)
    | `Name (_,tree) -> tree_height_diff tree
    )

  let rotate_right : KvSt.Tree.Data.t -> KvSt.Tree.Data.t =
    let mfn = TArt.mk_mfn (Name.gensym "rotate_right")
      (module KvSt.Tree.Data)
      ( fun r tree ->
        let rotate_right t = r.TArt.mfn_data t in
        ( match tree with
        | `Leaf _ -> failwith "impossible rr1"
        | `Bin(t1, x, t2) ->
          let rec loop = function
            | `Leaf _ -> failwith "impossible rr2"
            | `Bin(t21, y, t22) -> `Bin(`Bin(t1, x, t21), y, t22)              
            | `Art art -> loop (TArt.force art)
            | `Name(_, t) -> loop t
          in loop t2
          
        | `Art art -> rotate_right (TArt.force art)
        | `Name(nm, t) -> 
          if false then let nm1,nm2 = Name.fork nm in
                        `Name(nm1, `Art(r.TArt.mfn_nart nm2 t))
          else `Name(nm, rotate_right t)
        ))
    in
    fun tree -> mfn.TArt.mfn_data tree
      
  let rec rotate_left : KvSt.Tree.Data.t -> KvSt.Tree.Data.t =
    let mfn = TArt.mk_mfn (Name.gensym "rotate_left")
      (module KvSt.Tree.Data)
      ( fun r tree ->
        let rotate_left t = r.TArt.mfn_data t in
        ( match tree with
        | `Leaf _ -> failwith "impossible rl1"
        | `Bin(t1, x, t2) ->
          let rec loop = function
            | `Leaf _ -> failwith "impossible rl2"
            | `Bin(t11, y, t12) -> `Bin(t11, y, `Bin(t12, x, t2))
            | `Art art -> loop (TArt.force art)
            | `Name(_, t) -> loop t
          in loop t1
          
        | `Art art -> rotate_left (TArt.force art)
        | `Name(nm, t) -> 
          if false then
            let nm1,nm2 = Name.fork nm in
            `Name(nm1, `Art(r.TArt.mfn_nart nm2 t))
          else `Name(nm, rotate_left t)
        ))
    in
    fun tree -> mfn.TArt.mfn_data tree
      
  let nm_tree : Name.t -> KvSt.Tree.Data.t -> KvSt.Tree.Data.t =
    let mfn = TArt.mk_mfn (Name.gensym "nm_tree") 
      (module KvSt.Tree.Data) 
      (fun r tree -> tree)
    in 
    fun nm tree -> 
      let nm1,nm2 = Name.fork nm in
      `Name(nm1, `Art (mfn.TArt.mfn_nart nm2 tree))

  let rec avl_insert : Name.t -> KvSt.Tree.Data.t -> Kv.t -> KvSt.Tree.Data.t
    =
    let mfn = TArt.mk_mfn (Name.gensym "avl_insert")
      (module (Types.Tuple3(Name)(KvSt.Tree.Data)(Kv)))
      (fun r (insert_nm,tree,kv) ->
        let avl_insert nm tree kv = r.TArt.mfn_data (nm,tree,kv) in
        let wrap_avl nm tree =
          let h = tree_height_diff tree in
          assert (h = 0 || h = 1 || h = -1);
          (nm_tree nm tree)
        in
        ( match tree with              
        | `Art art -> avl_insert insert_nm (KvSt.Tree.Art.force art) kv
        | `Name(tree_nm, tree) -> avl_insert insert_nm tree kv
        | `Leaf `Nil -> nm_tree insert_nm (`Bin (`Leaf `Nil, kv, `Leaf `Nil))
        | `Leaf _ -> failwith "avl_insert: `Leaf _ : invalid AVL tree"
        | `Bin(left, kv0, right) ->
          let insert_nm1, insert_nm2 = Name.fork insert_nm in
          let ord = KeyData.compare (get_key kv) (get_key kv0) in
          if ord = 0 then
            `Bin(left, kv0, right)
          else if ord < 0 then
            let left' = avl_insert insert_nm1 left kv in
            let tree' = `Bin(left',kv0, right) in
            begin match tree_height_diff tree' with
            | -1 | 0 | 1 -> wrap_avl insert_nm2 tree'
            | 2 -> begin match tree_height_diff left' with
              | 1 -> wrap_avl insert_nm2 (rotate_left tree')
              | -1 -> let tree'' = `Bin(rotate_right left', kv0, right) in
                      wrap_avl insert_nm2 (rotate_left tree'')
              | _ -> failwith "impossible il1"
            end
            | _ -> failwith "impossible il2"
            end
          else if ord > 0 then
            let right' = avl_insert insert_nm1 right kv in
            let tree' = `Bin(left, kv0, right') in
            begin match tree_height_diff tree' with
            | -1 | 0 | 1 -> wrap_avl insert_nm2 tree'
            | -2 -> begin match tree_height_diff right' with
              | -1 -> wrap_avl insert_nm2 (rotate_right tree')
              | 1 -> let tree'' = `Bin(left, kv0, rotate_left right') in
                     wrap_avl insert_nm2 (rotate_right tree'')
              | _ -> failwith "impossible ir1"
            end
            | _ -> failwith "impossible ir2"
            end
          else
            failwith "impossible ilast"
        ))
    in
    fun nm tree kv -> 
      let nm1, nm2 = Name.fork nm in
      TArt.force (mfn.TArt.mfn_nart nm1 (nm2, tree, kv))


  let avl_tree_of_rope : Name.t -> KvSt.Rope.Data.t -> KvSt.Tree.Data.t -> KvSt.Tree.Data.t
    =
    let mfn = TArt.mk_mfn (Name.gensym "avl_tree_of_rope")
      (module Types.Tuple3(Name)(KvSt.Rope.Data)(KvSt.Tree.Data))
      (fun r (nm, rope, tree) ->
        let avl_tree_of_rope nm rope tree = r.TArt.mfn_data (nm, rope, tree) in
        (match rope with
        | `Zero -> tree
        | `One kv -> (*>>>  *) avl_insert nm tree kv
        | `Two (rope1, rope2) -> 
          let nm1, nm2 = Name.fork nm in
          let tree' = (avl_tree_of_rope nm1 rope1 tree) in
          avl_tree_of_rope nm2 rope2 tree'
        | `Art(art) -> avl_tree_of_rope nm (KvSt.Rope.Art.force art) tree
        | `Name(nm, rope) -> 
          let nm1, nm2 = Name.fork nm in
          TArt.force (r.TArt.mfn_nart nm1 (nm2,rope,tree))
        ))
    in
    fun nm rope tree -> mfn.TArt.mfn_data (nm, rope, tree)

  let avl_tree_of_list : Name.t -> KvSt.List.Data.t -> KvSt.Tree.Data.t -> KvSt.Tree.Data.t
    =
    let mfn = TArt.mk_mfn (Name.gensym "avl_tree_of_list")
      (module Types.Tuple3(Name)(KvSt.List.Data)(KvSt.Tree.Data))
      (fun r (nm, list, tree) ->
        let avl_tree_of_list nm list tree = r.TArt.mfn_data (nm, list, tree) in
        (match list with
        | `Nil -> tree
        
        | `Cons(x, tl) ->           
          let nm1,nm2 = Name.fork nm in
          let tree' = avl_insert nm1 tree x in
          avl_tree_of_list nm2 tl tree'

        | `Name(nm_here, tl) -> avl_tree_of_list nm_here tl tree

        | `Art(art) -> avl_tree_of_list nm (KvSt.List.Art.force art) tree
        ))
    in
    fun nm list tree -> mfn.TArt.mfn_data (nm, list, tree)

(*
  let hash_trie_of_list =
    match list with
    | `Nil -> `Leaf `Nil
    | `Cons(x, xs) -> let hash = Data.hash 0 x in
*)
end

(*
(* Directed graph representation. *)
module MakeDigraph
  ( ArtLib   : ArtLibType )
  ( Name     : NameType )
  ( NodeData : sig include DatType val compare : t -> t -> int end )
  ( EdgeData : DatType )
  =
struct
  module ArtLib   = ArtLib
  module Name     = Name
  module NodeData = NodeData
  module EdgeData = EdgeData

  module Edge = Types.Tuple3(NodeData)(EdgeData)(NodeData)
  module Adj  = Types.Tuple2(EdgeData)(NodeData)

  module NodeSt = MakeSpreadTree (ArtLib) (Name) (NodeData)
  module AdjSt  = MakeSpreadTree (ArtLib) (Name) (Adj)
  module EdgeSt = MakeSpreadTree (ArtLib) (Name) (Edge)

  module NodeSeq = MakeSeq (NodeSt)
  module AdjSeq  = MakeSeq (AdjSt)
  module EdgeSeq = MakeSeq (EdgeSt)
  module NodeMap = MakeKvMap (ArtLib) (Name) (NodeData) (AdjSt)

  let graph_without_node
      ( graph : NodeMap.KvSt.Tree.Data.t )
      ( node : NodeData.t )
      : NodeMap.KvSt.Tree.Data.t * NodeMap.KvSt.Data.t option =
    let node_op, graph = NodeMap.tree_remove graph node in
    ( graph, node_op )

  let tgt_nodes_of_adjs
      ( adjs : AdjSt.List.Data.t )
      : NodeSt.List.Data.t
      = failwith "TODO"

  let rec dfs
      ( graph : NodeMap.KvSt.Tree.Data.t )
      ( stack : NodeSt.List.Data.t )
      : NodeMap.KvSt.List.Data.t
      =
    ( match stack with
    | `Nil -> `Nil
    | `Art( art ) ->dfs graph (NodeSt.List.Art.force art)
    | `Cons(nd, stack_tl) ->
      ( match graph_without_node graph nd with
        (* node is already visited: *)
      | graph, None -> dfs graph stack_tl
        (* node is not yet visited: *)
      | graph, Some nd ->
        let stack' = NodeSeq.list_append
          (tgt_nodes_of_adjs (snd nd)) stack_tl
        in
        `Cons(nd, dfs graph stack')
      )
    | `Name _ -> failwith "TODO: Missing case"
    )

  let rec bfs
      ( graph : NodeMap.KvSt.Tree.Data.t )
      ( queue : NodeSt.Tree.Data.t )
      : NodeMap.KvSt.List.Data.t
      =
    ( match NodeSeq.tree_pop_front queue with
    | None -> `Nil
    | Some ( queue_tl, front_nd ) ->
      ( match graph_without_node graph front_nd with
        (* node is already visited: *)
      | graph, None -> bfs graph queue_tl
        (* node is not yet visited: *)
      | graph, Some nd ->
        let queue' = NodeSeq.tree_append
          queue_tl (NodeSeq.tree_of_list (tgt_nodes_of_adjs (snd nd)))
        in
        `Cons(nd, bfs graph queue')
      )
      (*      | `Art( art ) ->
              `Art( NodeMap.KvSt.List.Art.cell (
              dfs graph (NodeSt.List.Art.force art) ) ) *)
    )
end
*)

module ExprLang = struct
  
  module type ExprLangType = sig
    module ArtLib : ArtLibType
    module Name : NameType
    module Value : DatType
    
    type binop = string * (Value.t -> Value.t -> Value.t)
    type uniop = string * (Value.t -> Value.t)
    
    (* articulated expression for an arithmetic language with 'let'. *)
    type 'art art_expr = [   
       | `Let of string * 'art art_expr * 'art art_expr
    | `Var of string
    | `Value of Value.t
    | `Binop of binop * 'art art_expr * 'art art_expr
    | `Uniop of uniop * 'art art_expr
    | `Art of 'art
    | `Name of Name.t * 'art art_expr
    ]      
    module rec Expr : sig
      module Data : DatType
      module Art  : ArtType
    end
      with type Data.t = Expr.Art.t art_expr
      and type Art.Data.t = Expr.Art.t art_expr
      and type Art.Name.t = Name.t
  end
    
  module Make
    (ArtLib : ArtLibType)
    (Name   : NameType)
    (Value  : DatType) : ExprLangType = struct
      module ArtLib = ArtLib
      module Name = Name
      module Value = Value
        
      type binop = string * (Value.t -> Value.t -> Value.t)
      type uniop = string * (Value.t -> Value.t)
        
      (* articulated expression for an arithmetic language with 'let'. *)
      type 'art art_expr = [   
      | `Let of string * 'art art_expr * 'art art_expr
      | `Var of string
      | `Value of Value.t
      | `Binop of binop * 'art art_expr * 'art art_expr
      | `Uniop of uniop * 'art art_expr
      | `Art of 'art
      | `Name of Name.t * 'art art_expr
      ]
        
      module rec Expr : sig
        module Data : DatType
        module Art : ArtType
      end
        with type Data.t = Expr.Art.t art_expr
        and type Art.Data.t = Expr.Art.t art_expr
        and type Art.Name.t = Name.t
              = 
      struct
        module Data = struct
          type t = Expr.Art.t art_expr
            
          let rec string exp =
            ( match exp with
            | `Value v -> Value.string v
            | `Binop ((s,_), e1, e2) -> "Binop("^s^","^string e1^","^string e2^")"
            | `Uniop ((s,_), e) -> "Uniop("^s^","^string e^")"
            | `Let (v,e1,e2) -> "Let("^v^","^string e1^","^string e2^")"
            | `Var v -> "Var("^v^")"
            | `Art a -> "Art("^Expr.Art.string a^")"
            | `Name(nm, e) -> "Name("^Name.string nm^","^string e^")"
            )
              
          let rec hash seed x =
            ( match x with
            | `Value v -> Value.hash seed v
            | `Binop((s,_), e1, e2) -> Hashtbl.seeded_hash (hash (hash seed e1) e2) s
            | `Uniop ((s,_), e) -> Hashtbl.seeded_hash (hash seed e) s
            | `Let(v,e1,e2) -> Hashtbl.seeded_hash (hash (hash seed e1) e2) v
            | `Var v -> Hashtbl.seeded_hash seed v
            | `Art a -> Expr.Art.hash seed a
            | `Name(nm, e) -> Name.hash (hash seed e) nm
            )
              
          let rec equal (exp1:Expr.Art.t art_expr) (exp2:Expr.Art.t art_expr) =
            ( match exp1, exp2 with
            | `Value v1, `Value v2 -> Value.equal v1 v2
            | `Binop((s1, _), e11, e12),
              `Binop((s2, _), e21, e22) -> s1 = s2 && equal e11 e21 && equal e12 e22
            | `Uniop((s1,_),e1), `Uniop((s2,_),e2) -> s1 = s2 && equal e1 e2
            | `Let(v1,e11,e12), `Let(v2,e21,e22) -> v1 = v2 && equal e11 e21 && equal e12 e22
            | `Var v1, `Var v2 -> v1 = v2
            | `Art a1, `Art a2 -> Expr.Art.equal a1 a2
            | `Name(nm1, e1), `Name(nm2, e2) -> Name.equal nm1 nm2 && equal e1 e2
            | _ -> false
            )
              
          let rec sanitize x =
            ( match x with
            | `Value v -> `Value (Value.sanitize v)
            | `Binop (binop, e1, e2) -> `Binop(binop, sanitize e1, sanitize e2)
            | `Uniop (uniop, e) -> `Uniop(uniop, sanitize e)
            | `Let(v, e1, e2) -> `Let(v, sanitize e1, sanitize e2)
            | `Var v -> `Var v
            | `Art a -> `Art (Expr.Art.sanitize a)
            | `Name (nm, e) -> `Name (Name.sanitize nm, sanitize e)
            )
        end
        module Art = ArtLib.MakeArt(Name)(Data)
      end
    end

  module MakeEval ( ExprLang : ExprLangType )
    = 
  struct    
    (* TODO: Write small-step evaluator, and driver loop. *)
    open ExprLang
    module VArt = ArtLib.MakeArt(Name)(Value)
    module VOptionArt = ArtLib.MakeArt(Name)(Types.Option(Value))

    module Values = MakeSpreadTree(ArtLib)(Name)(Value)
    module Env = MakeKvMap(ArtLib)(Name)(Types.String)(Values)

    type env = Env.KvSt.Tree.Data.t
    type expr = Expr.Data.t
    type value = Value.t
    type nm = Name.t

    let eval_big : nm -> env -> expr -> value option
      = 
      let mfn = VOptionArt.mk_mfn (Name.gensym "eval_big")
        (module Types.Tuple3(Name)(Env.KvSt.Tree.Data)(Expr.Data))
        (fun r (nm,env,expr) ->
          let eval nm env exp = r.VOptionArt.mfn_data (nm,env,exp) in
          (match expr with
          | `Value v -> Some v
          
          | `Binop((_, binop), e1, e2) ->
            let nm1,nm2 = Name.fork nm in
            let v1 = eval nm1 env e1 in
            let v2 = eval nm2 env e2 in
            (match v1, v2 with
            | Some v1, Some v2 -> Some (binop v1 v2)
            | _ -> None
            )

          | `Uniop((_, uniop), e) ->
            let v = eval nm env e in
            (match v with
            | Some v -> Some (uniop v)
            | _ -> None
            )

(*              
          | `Let(var, e1, e2) ->
            let nm1, nm = Name.fork nm in
            let nm2, nm3 = Name.fork nm in
            let v1 = eval nm1 env e1 in
            let vs = match v1 with
              | None -> `Nil
              | Some v -> `Cons(v, `Nil) in
            (* let env' = Env.avl_insert nm2 env (var, vs) in *)
            (* TEMP *)
             let env' = Env.avl_insert nm2 env var in
            eval nm3 env' e2

          | `Var v -> ( match Env.tree_find env v with
            | Some (_, `Cons(value,_)) -> Some value
            | _ -> None
          )
*)          
          | `Art a -> eval nm env (Expr.Art.force a)
          | `Name(nm, exp) ->
            let nm1, nm2 = Name.fork nm in
            VOptionArt.force (r.VOptionArt.mfn_nart nm1 (nm2,env,exp))

          | _ -> failwith "FIXME"
          ))
      in
      fun nm env exp -> mfn.VOptionArt.mfn_data (nm, env, exp)


(*
    type cxt = [ `Cxt_Union of env * uniop
               | `Cxt_Binop of env * binop * expr
               | `Cxt_Let of env * string * expr 
               ]

    let eval_small : nm -> cxt -> env -> expr -> (cxt, env, expr) option
      = 
      let mfn = VOptionArt.mk_mfn (Name.gensym "eval")
        (module Types.Tuple3(Name)(Env.KvSt.Tree.Data)(Expr.Data))
        (fun r (nm,cxt,env,expr) ->          
          let eval nm env exp = r.VOptionArt.mfn_data (nm,cxt,env,exp) in
          (match expr with
          | None -> (cxt, env, None)
          | Some expr -> 
            (match expr with 
            | `Value v -> 
              let cxt_subst cxt v = match cxt with
                | `Cxt_Uniop(env, (s,uniop)) -> `Cxt_emp, env, Some (`Value (uniop v))
                | `Cxt_Binop(env, (s,binop), e) -> `Cxt_uniop(s,binop v), env, Some e
                | `Cxt_Let(env, var, e2) -> `Cxt_emp, (env(*TODO*)), Some e2
              in
              let cxt, env, expr = cxt_subst cxt v in
              (cxt, env, expr)
                
            | `Var var ->
              ( match tree_find env var with
              | Some v -> Some (`Value v)
              | None   -> None
              )
                  
            | `Binop((s, binop), e1, e2) ->
              (`Cxt_Binop(env, (s, binop), e2), env, Some e1)
                
            | `Uniop((s, uniop), e) ->
              (`Cxt_Uniop(env, (s, uniop)), env, Some e)
                
            | `Let(var, e1, e2) ->
              (`Cxt_Let(env, var, e2), env, Some e1)
                
            | `Art a -> (cxt, env, Some (Expr.Art.force a))
              
            | `Name(nm, exp) ->
              let nm1, nm2 = Name.fork nm in
              VOptionArt.force (r.VOptionArt.mfn_nart nm1 (nm2,env,exp))
            )))
      in
      fun nm cxt env exp -> mfn.VOptionArt.mfn_data (nm,cxt,env,exp)
*)
    
  end    
end
  
