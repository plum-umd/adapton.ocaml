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

open Primitives
open GrifolaType

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
    =
  struct
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
module MakeSeq ( St : SpreadTreeType )
  =
struct
  module Name        = St.Name
  module AData       = St.ArtLib.MakeArt(Name)(St.Data)
  module ADataOption = St.ArtLib.MakeArt(Name)(Types.Option(St.Data))

  (* Abbreviations, for accessing mfn_* and force: *)
  module LArt = St.List.Art
  module TArt = St.Tree.Art
  module RArt = St.Rope.Art

  let mut_elms_of_list
      ( name : Name.t )
      ( list : 'a list )
      ( data_of : 'a -> St.Data.t )
      ( name_of : 'a -> Name.t ) : St.List.Art.t = 
    let rec loop list = match list with
      | [] -> `Nil
      | x :: xs -> 
        let nm1, nm2 = Name.fork (name_of x) in
        `Cons((data_of x), `Name(nm1, `Art (St.List.Art.cell nm2 (loop xs))))
    in St.List.Art.cell name (loop list)

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

  (* OLD version: *)
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
            let nm2, nm  = Name.fork nm in
            let nm3, nm4 = Name.fork nm in
            let tree_rest = r.P.Art.mfn_nart nm1 (parent_lev,tree_lev,tree,list) in
            let tree,rest = P.split nm2 tree_rest in
            (`Name(nm3, `Art tree), `Name(nm4, `Art rest))
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

(*
  (* VVVVVVVVVVVVVV *)
  let list_hashbal_reduce 
      (binop_nm : Name.t) 
      (binop : St.Data.t -> St.Data.t -> St.Data.t) : St.List.Data.t -> St.Data.t =
    let module M = (* *) in
    let mfn = M.mk_mfn (St.Name.gensym "list_hashbal_reduce")
      (module Types.Tuple5(Types.Option(Name))(Types.Int)(Types.Int)(Types.Option(St.Tree.Data))(St.List.Data))
      (fun r accum list ->
        let tree_of_list_rec nmopt ul ll accum list = r.M.mfn_data (nmopt, accum, list) in
        ( match list with
        | `Nil -> nmopt, accum, list
        | `Cons (hd, tl) -> (
          let ffs = AdaptonInternal.Primitives.ffs in
          let hd_lev = ffs (St.Data.hash 0 hd) in
          if not (ll <= hd_lev && hd_lev <= ul) then
            (nmopt, accum, list)
          else match nmopt with
          | None ->
            let nmopt, accum, rest = list_hashbal_reduce None hd_lev (-1) None tl in
            list_hashbal_reduce nmopt ul hd_lev accum rest
          | Some nm ->
            let nm1, nm  = Name.fork nm in
            let nm2, nm3 = Name.fork nm in
            let art = r.M.mfn_nart nm1 (None, ul, ll, accum, list) in
            M.force art in
        )
        | `Art art -> list_hashbal_reduce nmopt ul ll accum (St.List.Art.force art)
        | `Name(nm, list) -> list_hashbal_reduce (Some nm) ul ll accum list
        ))
    in
    fun op list -> 
      let _, accum, list = mfn.M.mfn_data (None, max_int, -1, None, list) in
      match list with
      | `Nil -> accum
      | _ -> failwith "tree_of_list_eager_rec: impossible"
*)

  (* !!!!!!!!! *)
  (* Current version. *)
  (* Overview example. *)
  let tree_of_list_eager : St.List.Data.t -> St.Tree.Data.t =
    let module M = St.ArtLib.MakeArt(Name)(Types.Tuple3(Types.Option(Name))(St.Tree.Data)(St.List.Data)) in
    let mfn = M.mk_mfn (St.Name.gensym "tree_of_list_rec")
      (module Types.Tuple5(Types.Option(Name))(Types.Int)(Types.Int)(St.Tree.Data)(St.List.Data))
      (fun r (nmopt, parent_lev, tree_lev, tree, list) ->
        let _ = failwith "XXX" in
        let tree_of_list_rec nmopt pl tl t l = r.M.mfn_data (nmopt, pl,tl,t,l) in
        ( match list with
        | `Nil -> nmopt, tree, `Nil
        | `Cons (hd, tl) -> (
          let ffs = Primitives.ffs in
          let hd_lev = ffs (St.Data.hash 0 hd) in
          if not (tree_lev <= hd_lev && hd_lev <= parent_lev) then
            nmopt, tree, list
          else match nmopt with
          | None ->
            let nmopt, right, rest = tree_of_list_rec None hd_lev (-1) (`Leaf `Nil) tl in
            let tree = `Bin(tree, hd, right) in
            tree_of_list_rec nmopt parent_lev hd_lev tree rest
          | Some nm ->
            let nm1, nm  = Name.fork nm in
            let nm2, nm3 = Name.fork nm in
            let art = r.M.mfn_nart nm1 (None, parent_lev,tree_lev,tree,list) in
            let nmopt, tree, rest = M.force art in
            nmopt, `Name(nm3, `Art (St.Tree.Art.cell nm2 tree)), rest
        )
        | `Art art -> tree_of_list_rec nmopt parent_lev tree_lev tree (St.List.Art.force art)
        | `Name(nm, list) -> 
          tree_of_list_rec (Some nm) parent_lev tree_lev tree list
        ))
    in
    fun list -> 
      let _, tree, list = mfn.M.mfn_data (None, max_int, -1, `Leaf `Nil, list) in
      match list with
      | `Nil -> tree
      | _ -> failwith "tree_of_list_eager_rec: impossible"


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
            let nm2, nm  = Name.fork nm in
            let nm3, nm4 = Name.fork nm in
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

  (* TODO: Debug this: Esp, granularity control parameters. *)
  let rope_of_list_nm : art_threshold:int -> St.List.Data.t -> St.Rope.Data.t =
    (*let module M = St.ArtLib.MakeArt(Name)(Types.Tuple2(St.Rope.Data)(St.List.Data)) in*)
    let module P = St.ArtLib.MakeArtTuple2(Name)(St.Rope.Art)(St.List.Art) in
    fun ~art_threshold ->
      let rope_of_list_nm_rec = (
        let mfn = P.Art.mk_mfn (St.Name.gensym "rope_of_list_rec")
          (module Types.Tuple4(Types.Int)(Types.Int)(St.Rope.Data)(St.List.Data))
          (fun r (parent_lev, rope_lev, rope, list) ->
            let rope_of_list_nm_rec pl tl t l = r.P.Art.mfn_data (pl,tl,t,l) in
            ( match list with
            | `Nil -> rope, `Nil
            | `Cons (hd, tl) -> rope_of_list_nm_rec parent_lev rope_lev (`Two(rope,`One hd)) tl
            | `Art art -> rope_of_list_nm_rec parent_lev rope_lev rope (St.List.Art.force art)
            | `Name(nm, list) ->             
              let nm_lev = Name.height nm in
              if false (* nm_lev < art_threshold *) then (
                (* TODO: Fix Performance bug here. *)
                if rope_lev <= nm_lev && nm_lev <= parent_lev then (
                  let right, rest = rope_of_list_nm_rec nm_lev (-1) (`Zero) list in
                  let rope = `Two(rope,right) in
                  let rope,rest = rope_of_list_nm_rec parent_lev nm_lev rope rest in
                  (`Name(nm, rope)), rest
                )
                else
                  rope, list
              ) 
              else (
                if rope_lev <= nm_lev && nm_lev <= parent_lev then (
                  let nm1, nm = Name.fork nm in
                  let right, rest = P.Art.force (r.P.Art.mfn_nart nm1 (nm_lev,-1,`Zero,list)) in
                  let rope = `Two(rope, right) in
                  let nm2, nm  = Name.fork nm in
                  let nm3, nm4 = Name.fork nm in
                  let art = r.P.Art.mfn_nart nm2 (parent_lev,nm_lev,rope,rest) in
                  let rope,rest = P.Art.force art in
                  (`Name(nm3, `Art (P.fst nm4 art)), rest)
                ) else (
                  rope, list
                ))))
        in
        fun list ->           
          let rope, rest = mfn.P.Art.mfn_data (max_int, -1, `Zero, list) in
          (* assert (list_is_empty rest) ; *)
          rope
      ) in
      rope_of_list_nm_rec

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


  let list_of_rope_nm : art_threshold:int -> St.Rope.Data.t -> St.List.Data.t -> St.List.Data.t =
    fun ~art_threshold ->      
      let mfn = LArt.mk_mfn (St.Name.gensym "list_of_rope")      
        (module Types.Tuple2(St.Rope.Data)(St.List.Data))
        (fun r (rope, rest) ->
          let list_of_rope rope list = r.LArt.mfn_data (rope, list) in
          ( match rope with
          | `Zero          -> rest
          | `One x         -> `Cons(x, rest)
          | `Two(x,y)      -> list_of_rope x (list_of_rope y rest)
          | `Art art       -> list_of_rope (RArt.force art) rest
          | `Name(nm,rope) -> 
            if Name.height nm < art_threshold then
              `Name(nm, list_of_rope rope rest)
            else
              let nm1,nm2 = Name.fork nm in
              `Name(nm1, `Art(r.LArt.mfn_nart nm2 (rope, rest)))
          ))
      in
      fun rope list -> mfn.LArt.mfn_data (rope, list)

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
          ADataOption.force (r.ADataOption.mfn_nart nm list)
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

  let list_merge
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.List.Data.t -> St.List.Data.t -> St.List.Data.t =
    let fnn = St.Name.pair (St.Name.gensym "list_merge") compare_nm in
    let mfn = St.List.Art.mk_mfn fnn
      (module (Types.Tuple2(St.List.Data)(St.List.Data)))
      (fun r (list1,list2) ->
        let list_merge xs ys = r.LArt.mfn_data (xs,ys) in
        ( match list1, list2 with
        | `Nil, _ -> list2
        | _, `Nil -> list1
        | `Cons(x, xs), `Cons(y, ys) ->
          incr Statistics.Counts.unit_cost ;
          if compare x y <= 0 then 
            `Cons(x, (list_merge xs list2))
          else
            `Cons(y, (list_merge list1 ys))
        
        | `Art a, _ -> list_merge (LArt.force a) list2
        | _, `Art a -> list_merge list1 (LArt.force a)
        | `Name(nm, xs), ys | xs, `Name(nm, ys) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art( r.LArt.mfn_nart nm2 (xs, ys) ))
        ))
    in
    fun xs ys -> mfn.LArt.mfn_data (xs,ys)

  let list_merge_nm ~art_threshold 
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.List.Data.t -> St.List.Data.t -> St.List.Data.t =
    let fnn = St.Name.pair (St.Name.gensym "list_merge") compare_nm in
    let mfn = St.List.Art.mk_mfn fnn
      (module (Types.Tuple2(St.List.Data)(St.List.Data)))
      (fun r (list1,list2) ->
        let list_merge xs ys = r.LArt.mfn_data (xs,ys) in
        ( match list1, list2 with
        | `Nil, _ -> list2
        | _, `Nil -> list1
        | `Cons(x, xs), `Cons(y, ys) ->
          incr Statistics.Counts.unit_cost ;
          if compare x y <= 0 then 
            `Cons(x, (list_merge xs list2))
          else
            `Cons(y, (list_merge list1 ys))
        
        | `Art a, _ -> list_merge (LArt.force a) list2
        | _, `Art a -> list_merge list1 (LArt.force a)
        | `Name(nm, xs), ys | xs, `Name(nm, ys) -> 
          if Name.height nm < art_threshold then 
            `Name(nm, list_merge xs ys)
          else (
            let nm1, nm2 = Name.fork nm in
            `Name(nm1, `Art( r.LArt.mfn_nart nm2 (xs, ys) ))
          )))
    in
    fun xs ys -> mfn.LArt.mfn_data (xs,ys)

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

  let rope_mergesort_nm ~art_threshold
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.Rope.Data.t -> St.List.Data.t =
    let fnn = St.Name.pair (St.Name.gensym "rope_mergesort_nm") compare_nm in    
    let merge = 
      if true then 
        list_merge_nm ~art_threshold compare_nm compare 
      else 
        list_merge compare_nm compare 
    in
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
          if Name.height nm < art_threshold then 
            `Name(nm, rope_mergesort rope)
          else
            let nm1,nm2 = Name.fork nm in
            `Name(nm1, `Art (r.LArt.mfn_nart nm2 rope))            
        ))
    in
    fun rope -> mfn.LArt.mfn_data rope

  let list_mergesort_nm ~rope_art_threshold ~list_art_threshold
      ( compare_nm : St.Name.t )
      ( compare : St.Data.t -> St.Data.t -> int )
      : St.List.Data.t -> St.List.Data.t =
    let sort = 
      if true then rope_mergesort_nm
        ~art_threshold:list_art_threshold 
        compare_nm compare 
      else rope_mergesort compare_nm compare 
    in    
    fun list ->
      let rope = rope_of_list_nm ~art_threshold:rope_art_threshold list in
      sort rope

  let list_mergesort 
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
  
