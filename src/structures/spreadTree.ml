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
open Shared

module type S = sig
  type elt
  type name

  type 'art art_list = [ (* articulated list. *)
  | `Nil
  | `Cons of elt * 'art art_list
  | `Art of 'art
  | `Name of name * 'art art_list
  ]

  type ('leaf, 'art) art_tree = [ (* articulated tree. *)
  | `Leaf of 'leaf
  | `Bin of ('leaf,'art) art_tree * elt * ('leaf,'art) art_tree
  | `Art of 'art
  | `Name of name * ('leaf, 'art) art_tree
  ]

  type ('one, 'art) art_rope = [ (* articulated rope. *)
  | `Zero
  | `One of 'one
  | `Two of ('one,'art) art_rope * ('one,'art) art_rope
  | `Art of 'art
  | `Name of name * ('one,'art) art_rope
  ]

  module rec List : Adapted.S with type    t = List.Art.t art_list
                               and type name = name

  module rec Tree : Adapted.S with type    t = (List.t, Tree.Art.t) art_tree
                               and type name = name

  module rec Rope : Adapted.S with type    t = (elt, Rope.Art.t) art_rope
                               and type name = name

end


module Make
  (ArtLib : ArtLib.S)
  (Name   : Name.S)
  (Elt    : Data.S)
  : S with type name = Name.t
       and type  elt = Elt.t =
struct
  type  elt = Elt.t  [@@deriving eq, ord, show]
  type name = Name.t [@@deriving eq, ord, show]

  type 'art art_list = [ (* articulated list. *)
  | `Nil
  | `Cons of elt * 'art art_list
  | `Art of 'art
  | `Name of Name.t * 'art art_list
  ]
  [@@deriving eq, ord, show]

  type ('leaf, 'art) art_tree = [ (* articulated tree. *)
  | `Leaf of 'leaf
  | `Bin of ('leaf,'art) art_tree * elt * ('leaf,'art) art_tree
  | `Art of 'art
  | `Name of Name.t * ('leaf,'art) art_tree
  ]
  [@@deriving eq, ord, show]

  type ('one, 'art) art_rope = [ (* articulated rope. *)
  | `Zero
  | `One of 'one
  | `Two of ('one,'art) art_rope * ('one,'art) art_rope
  | `Art of 'art
  | `Name of Name.t * ('one,'art) art_rope
  ]
  [@@deriving eq, ord, show]

  module rec List : (Adapted.S with type    t = List.Art.t art_list
                                and type name = Name.t) =
  struct
    type name = Name.t
    module Data = struct
      type t = List.Art.t art_list
      [@@deriving eq, ord, show]

      let rec hash seed x =
        ( match x with
        | `Nil -> Hashtbl.seeded_hash seed `Nil
        | `Cons(x,tl) -> Elt.hash (hash seed tl) x
        | `Art a -> List.Art.hash seed a
        | `Name(nm,xs) -> (Name.hash (hash seed xs) nm)
        )

      let rec sanitize x =
        ( match x with
        | `Nil -> `Nil
        | `Cons (x, tl) -> `Cons(Elt.sanitize x, sanitize tl)
        | `Art a -> `Art (List.Art.sanitize a)
        | `Name(nm,xs) -> `Name(Name.sanitize nm, sanitize xs)
        )
    end
    module Art = ArtLib.MakeArt(Name)(Data)
    include Data
  end

  module rec Tree : (Adapted.S with type    t = (List.t, Tree.Art.t) art_tree
                                and type name = Name.t) =
  struct
    type name = Name.t
    module Data = struct
      type t = (List.t, Tree.Art.t) art_tree
      [@@deriving eq, ord, show]

      let rec hash seed x =
        ( match x with
        | `Leaf xs -> List.hash seed xs
        | `Bin(l,x,r) -> hash (Elt.hash (hash seed l) x) r
        | `Art a -> Tree.Art.hash seed a
        | `Name(nm,x) -> (Name.hash (hash seed x) nm)
        )

      let rec sanitize x =
        ( match x with
        | `Leaf x -> `Leaf (List.sanitize x)
        | `Bin(l,x,r) -> `Bin(sanitize l, Elt.sanitize x, sanitize r)
        | `Art a -> `Art(Tree.Art.sanitize a)
        | `Name(nm,x) -> `Name(Name.sanitize nm, sanitize x)
        )
    end
    module Art = ArtLib.MakeArt(Name)(Data)
    include Data
  end

  module rec Rope : (Adapted.S with type    t = (elt, Rope.Art.t) art_rope
                                and type name = Name.t) =
  struct
    type name = Name.t
    module Data = struct
      type t = (elt, Rope.Art.t) art_rope
      [@@deriving eq, ord, show]

      let rec hash seed x =
        ( match x with
        | `Zero -> 0
        | `One x -> Elt.hash seed x
        | `Two (x,y) -> hash (hash seed x) y
        | `Art a -> Rope.Art.hash seed a
        | `Name(nm,x) -> (Name.hash (hash seed x) nm)
        )

      let rec sanitize x =
        ( match x with
        | `Zero -> `Zero
        | `One x -> `One (Elt.sanitize x)
        | `Two(x,y) -> `Two (sanitize x, sanitize y)
        | `Art a -> `Art(Rope.Art.sanitize a)
        | `Name(nm,x) -> `Name(Name.sanitize nm, sanitize x)
        )
    end
    module Art = ArtLib.MakeArt(Name)(Data)
    include Data
  end
end


(* Sequences, based on SpreadTrees. *)
(* *)
module SeqWrap
  (ArtLib : ArtLib.S)
  (Name   : Name.S)
  (Elt    : Data.S)
  (St     : S with type  elt = Elt.t
               and type name = Name.t) =
struct

  let default_granularity = 4

  type name = St.name
  module AElt       = ArtLib.MakeArt(Name)(Elt)
  module AEltOption = ArtLib.MakeArt(Name)(Types.Option(Elt))

  (* Abbreviations, for accessing mfn_* and force: *)
  module LArt = St.List.Art
  module TArt = St.Tree.Art
  module RArt = St.Rope.Art

  let mut_elts_of_list
    ?c:(cons_first=true)
    ( name : name )
    ( list : 'a list )
    ( data_of : 'a -> St.elt )
    ( name_of : 'a -> name )
    ( gran_level : int )
    : St.List.Art.t
  =
    let rec loop list =
      match list with
      | [] -> `Nil
      | x :: xs ->
        if ffs (Elt.hash 0 (data_of x)) >= gran_level then
          let nm1, nm2 = Name.fork (name_of x) in
          if cons_first then
            `Cons((data_of x), `Name(nm1, `Art (St.List.Art.cell nm2 (loop xs))))
          else
            `Name(nm1, `Cons((data_of x), `Art (St.List.Art.cell nm2 (loop xs))))          
        else
          `Cons((data_of x), (loop xs))
    in St.List.Art.cell name (loop list)

  let simple_full_string =
    let rec loop = function
      | `Nil -> "Nil"
      | `Cons(x,xs) -> (Elt.show x)^"; "^(loop xs)
      | `Art(a) -> "Art => "^(loop (LArt.force a))
      | `Name(_,xs) -> "Name; "^(loop xs)
    in loop

  let rec insert_elt list_art h nm_tl_opt =
    match nm_tl_opt with
    | Some (nm, tl_art) ->
      assert ( list_art <> tl_art );
      let list_art_content = St.List.Art.force list_art in
      St.List.Art.set list_art (`Cons(h, `Name(nm, `Art(tl_art)))) ;
      St.List.Art.set tl_art list_art_content ;
    | None ->
      let list_art_content = St.List.Art.force list_art in
      St.List.Art.set list_art (`Cons(h, list_art_content))

  let rec delete_elt list_art =
    let (x,x_tl) =
      let rec loop list =
        match list with
        | `Art art ->
          let elt, tl = loop (St.List.Art.force art) in
          elt, (`Art art)

        | `Name (nm, tl) ->
          let elt, tl = loop tl in
          elt, (`Name(nm, tl))

        | `Cons(x, tl) -> (x,tl)
        | `Nil -> failwith "delete_elt: Nil: No element to delete"
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

  (*
  This function returns the final art of a list, which contains exactly `Nil
  if it's not available, it's created first, mutating the input list directly
  *)
  let get_or_create_final_art (list : LArt.t) =
    let rec find_last art =
      match next_art (LArt.force art) with
      | None -> art
      | Some(a) -> find_last a
    in
    let la = find_last list in
    (* return it if it already contains `Nil *)
    if LArt.force la = `Nil then la else
    let rec create_nil_art elt =
      match elt with
      | `Nil ->
        let nm1, nm2 = Name.fork (Name.nondet()) in
        `Name(nm1, `Art(LArt.cell nm2 `Nil))
      | `Cons(x,xs) -> `Cons(x, create_nil_art xs)
      | `Art(a) -> failwith "two last arts!"
      | `Name(nm, xs) -> `Name(nm, create_nil_art xs)
    in
    (* add articulated `Nil to the end and return that art *)
    LArt.set la (create_nil_art (LArt.force la));
    find_last la

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

  let rec list_is_empty ( list : St.List.t) : bool =
    ( match list with
    | `Nil -> true
    | `Cons(_,_) -> false
    | `Art a -> list_is_empty ( LArt.force a )
    | `Name (_,x) -> list_is_empty x
    )

  let list_length : St.List.t -> int =
    let module Len = ArtLib.MakeArt(Name)(Types.Int) in
    let mfn = Len.mk_mfn (Name.gensym "list_length")
      (module St.List)
      (fun r l ->
        let len l = r.Len.mfn_data l in
        let memo_len n l = r.Len.mfn_nart n l in
        match l with
        | `Nil -> 0
        | `Cons(_,l) -> 1 + (len l)
        | `Art(a) -> len (LArt.force a)
        | `Name(nm, l) -> Len.force (memo_len nm l)
      )
    in
    fun l -> mfn.Len.mfn_data l


  let list_append =
    let mfn = LArt.mk_mfn (Name.gensym "list_append")
      (module Types.Tuple2(St.List)(St.List))
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

  let list_of_tree : St.Tree.t -> St.List.t -> St.List.t =
    let mfn = LArt.mk_mfn (Name.gensym "list_of_tree")
      (module Types.Tuple2(St.Tree)(St.List))
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

  let rope_of_list_rec : name option -> int -> int -> St.Rope.t -> St.List.t -> St.Rope.t * St.List.t =
    let module P = Adapted.ArtTuple2(ArtLib)(Name)(St.Rope)(St.List) in
    let rope_of_list_rec =
      let mfn = P.Art.mk_mfn (Name.gensym "rope_of_list_rec")
        (module Types.Tuple5(Types.Option(Name))(Types.Int)(Types.Int)(St.Rope)(St.List))

        (fun r (nm_opt, parent_lev, rope_lev, rope, list) ->
          let rope_of_list_rec no pl tl t l = r.P.Art.mfn_data (no,pl,tl,t,l) in
          ( match list with
          | `Nil -> rope, `Nil
          | `Cons (hd, tl) ->
            let hd_lev = ffs (Elt.hash 0 hd) in
            if rope_lev <= hd_lev && hd_lev <= parent_lev then (
              match nm_opt with
              | None ->
                let right, rest = rope_of_list_rec None hd_lev (-1) (`One hd) tl in
                let rope = `Two(rope, right) in
                rope_of_list_rec None parent_lev hd_lev rope rest
              | Some(nm) ->
                let nm1,nm  = Name.fork nm in
                let nm2,nm3 = Name.fork nm in
                let right, rest = P.split nm1 (r.P.Art.mfn_nart nm2 (None, hd_lev, (-1), (`One hd), tl)) in
                let rope : St.Rope.t = `Two(rope, `Name(nm3, `Art(right))) in
                rope_of_list_rec None parent_lev hd_lev rope (LArt.force rest)
            )
            else (
              match nm_opt with
              | None -> rope, list
              | Some(nm) -> rope, `Name(nm, list)
            )
          | `Art art -> rope_of_list_rec nm_opt parent_lev rope_lev rope (LArt.force art)
          | `Name(nm, list) -> rope_of_list_rec (Some nm) parent_lev rope_lev rope list
          )
        )
      in
      fun nm pl tl t l -> mfn.P.Art.mfn_data (nm, pl, tl, t, l)
    in
    rope_of_list_rec

  let rope_of_list : St.List.t -> St.Rope.t =
    fun list ->
      let rope, rest =
        rope_of_list_rec None max_int (-1) (`Zero) list
      in
      (* assert (list_is_empty rest) ; *)
      rope

  let list_of_rope : St.Rope.t -> St.List.t -> St.List.t =
    let mfn = LArt.mk_mfn (Name.gensym "list_of_rope")
      (module Types.Tuple2(St.Rope)(St.List))
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

  let rope_length : St.Rope.t -> int =
    let module Len = ArtLib.MakeArt(Name)(Types.Int) in
    let mfn = Len.mk_mfn (Name.gensym "rope_length")
      (module St.Rope)
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

  let rope_not_empty : name -> St.Rope.t -> bool =
    fun (namespace : name) ->
    let module M = ArtLib.MakeArt(Name)(Types.Bool) in
    let fnn = Name.pair (Name.gensym "rope_empty") namespace in
    let mfn = M.mk_mfn fnn
      (module St.Rope)
      (fun r rope ->
        let empty rope = r.M.mfn_data rope in
        let memo_empty n rope = r.M.mfn_nart n rope in
        match rope with
        | `Zero -> false
        | `One(x) -> true
        | `Two(r1,r2) -> (empty r1) || (empty r2)
        | `Art(a) -> empty (RArt.force a)
        | `Name(nm, r) -> M.force (memo_empty nm r)
      )
    in
    fun rope -> mfn.M.mfn_data rope

  (* non-memoised indexed lookup of a rope, using memoized rope_length for speed *)
  let rope_nth rope n : Elt.t option =
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

  let list_reverse : St.List.t -> St.List.t -> St.List.t =
    let mfn = LArt.mk_mfn (Name.gensym "list_reverse")
      (module Types.Tuple2(St.List)(St.List))
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

  let list_reverse_balanced : St.List.t -> St.List.t -> St.List.t =
    let debug = false in
    let accum =
      LArt.mk_mfn
        (Name.gensym "list_reverse_accum")
        (module St.List)
        (fun _ list ->
           (if debug then Printf.printf "... accum=(%s)\n" (St.List.show list));
           list)
    in
    let module Res = ArtLib.MakeArt(Name)(Types.Tuple2(St.List)(St.List)) in
    let module Arg = Types.Tuple5(Types.Option(Name))(Types.Int)(Types.Int)(St.List)(St.List) in
    let mfn =
      Res.mk_mfn
        (Name.gensym "list_reverse")(module Arg)
        (fun r ((no, lo, hi, list, rev) as arg) ->
         (if debug then Printf.printf "... list_reverse:args=(%s)\n%!" (Arg.show arg)) ;
         let list_reverse no lo hi list rev = r.Res.mfn_data (no,lo,hi,list,rev) in
         ( match list with
           | `Nil -> (`Nil, rev)
           | `Cons(x, xs) ->
              let hd_lev = Shared.ffs (Elt.hash 0 x) in
              if lo <= hd_lev && hd_lev <= hi then (
                match no with
                | None ->
                   let rev = `Cons(x,rev) in
                   let rest, rev = list_reverse None (-1) hd_lev xs rev in
                   (if debug then Printf.printf "... rest1,rev1 = %s,%s\n%!" (St.List.show rest) (St.List.show rev)) ;
                   let rest, rev = list_reverse None hd_lev hi rest rev in
                   (if debug then Printf.printf "... rest2,rev2 = %s,%s\n%!" (St.List.show rest) (St.List.show rev)) ;
                   rest, rev
                                
                | Some nm ->
                   let nm1,nm  = Name.fork nm in
                   let nm2,nm3 = Name.fork nm in
                   let rev = `Name(nm1, `Art(accum.LArt.mfn_nart nm2 (`Cons(x, rev)))) in
                   let rest, rev = Res.force (r.Res.mfn_nart nm3 (None, -1, hd_lev, xs, rev)) in
                   (if debug then Printf.printf "...N rest1,rev1 = %s,%s\n%!" (St.List.show rest) (St.List.show rev)) ;
                   let rest, rev = list_reverse None hd_lev hi rest rev in
                   (if debug then Printf.printf "...N rest2,rev2 = %s,%s\n%!" (St.List.show rest) (St.List.show rev)) ;
                   rest, rev
              )
              else (
                (if debug then Printf.printf "... Basecase: list,rev = %s,%s\n%!" (St.List.show list) (St.List.show rev)) ;
                match no with
                | Some nm -> (`Name(nm,list), rev)
                | None -> (list, rev)                                
              )
           | `Art art -> list_reverse no lo hi (LArt.force art) rev
           | `Name (nm, xs) -> list_reverse (Some nm) lo hi xs rev
         ))
    in
    fun list rev ->
    match mfn.Res.mfn_data (None, -1, max_int, list, rev) with
    | `Nil, rev -> rev
    | _, _ -> failwith "list_reverse: impossible"

  let rec rope_reverse =
    let mfn = RArt.mk_mfn (Name.gensym "rope_reverse")
      (module St.Rope)
      (fun r rope -> let rope_reverse = r.RArt.mfn_data in
        ( match rope with
        | `Zero -> `Zero
        | `One x -> `One x
        | `Two(x,y) -> `Two(rope_reverse y, rope_reverse x)
        | `Art art -> rope_reverse (RArt.force art)
        | `Name (nm,rope) ->
           let nm1,nm2 = Name.fork nm in
           let art = r.RArt.mfn_nart nm2 rope in
           ignore (RArt.force art) ;
          `Name(nm1, `Art(art))
        ))
    in
    fun rope -> mfn.RArt.mfn_data rope

  (* TODO: optimize, compact zeros *)
  let rope_filter
    (op_nm : name)
    (op : Elt.t -> bool)
    : St.Rope.t -> St.Rope.t = 
    let fnn = Name.pair (Name.gensym "rope_filter") op_nm in
    let mfn = RArt.mk_mfn fnn
      (module St.Rope)
      (fun r rope ->
        let rope_filter = r.RArt.mfn_data in
        match rope with
        | `Zero -> `Zero
        | `One(x) -> if (op x) then `One(x) else `Zero
        | `Two(x,y) -> `Two(rope_filter x, rope_filter y)
        | `Art(a) -> rope_filter (RArt.force a)
        | `Name(nm, rp) ->
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.RArt.mfn_nart nm2 rp))
      )
    in
    fun rope -> mfn.RArt.mfn_data rope

  let list_filter 
    (op_nm : name)
    (op : Elt.t -> bool)
    : St.List.t -> St.List.t = 
    let fnn = Name.pair (Name.gensym "list_filter") op_nm in
    let mfn = LArt.mk_mfn fnn
      (module St.List)
      (fun r list -> 
        let list_filter = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(x, xs) -> 
          let rest = list_filter xs in
          if op x then `Cons(x, rest) else rest
        | `Art(a) -> list_filter (LArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 xs))
      )
    in
    fun list -> mfn.LArt.mfn_data list

  let list_map 
    (op_nm : name)
    (op : Elt.t -> Elt.t)
    : St.List.t -> St.List.t = 
    let fnn = Name.pair (Name.gensym "list_map") op_nm in
    let mfn = LArt.mk_mfn fnn
      (module St.List)
      (fun r list -> 
        let list_map = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(x, xs) -> `Cons(op x, list_map xs)
        | `Art(a) -> list_map (LArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 xs))
      )
    in
    fun list -> mfn.LArt.mfn_data list

  let list_ref_cell
    : name -> St.List.t -> St.List.Art.t = 
    let fnn = Name.gensym "list_ref_cell" in
    let mfn = LArt.mk_mfn fnn
      (module St.List)
      (fun r list -> list)
    in
    mfn.St.List.Art.mfn_nart

  let list_eager_map 
    (op_nm : name)
    (op : Elt.t -> Elt.t)
    : St.List.t -> St.List.t = 
    let fnn = Name.pair (Name.gensym "list_map") op_nm in
    let mfn = LArt.mk_mfn fnn
      (module St.List)
      (fun r list -> 
        let list_map = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(x, xs) -> `Cons(op x, list_map xs)
        | `Art(a) -> list_map (LArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm  = Name.fork nm in
          let nm2, nm3 = Name.fork nm in
          let ys = (* memoized recursive call: *)
            LArt.force (r.LArt.mfn_nart nm1 xs)
          in
          let ref_ys = list_ref_cell nm2 ys in
          `Name(nm3, `Art(ref_ys))
      )
    in
    fun list -> mfn.LArt.mfn_data list

  let list_eager_filter 
    (op_nm : name)
    (op : Elt.t -> bool)
    : St.List.t -> St.List.t = 
    let fnn = Name.pair (Name.gensym "list_filter") op_nm in
    let mfn = LArt.mk_mfn fnn
      (module St.List)
      (fun r list -> 
        let list_filter = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(x, xs) -> 
          let rest = list_filter xs in
          if op x then `Cons(x, rest) else rest
        | `Art(a) -> list_filter (LArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm  = Name.fork nm in
          let nm2, nm3 = Name.fork nm in
          let ys = (* memoized recursive call: *)
            LArt.force (r.LArt.mfn_nart nm1 xs)
          in
          let ref_ys = list_ref_cell nm2 ys in
          `Name(nm3, `Art(ref_ys))
      )
    in
    fun list -> mfn.LArt.mfn_data list

  let list_map_paired
    (op_nm : name)
    (op : Elt.t -> Elt.t -> Elt.t)
    : St.List.t -> St.List.t =
    let fnn = Name.pair (Name.gensym "list_map_paired") op_nm in
    let mfn = LArt.mk_mfn fnn
      (module St.List)
      (fun r list ->
        let map2 = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        (* ignore last value if unpaired *)
        | `Cons(_, `Nil) -> `Nil
        | `Cons(x, `Cons(y, ys)) -> `Cons(op x y, map2 ys)
        | `Cons(x, `Art(a)) -> map2 (`Cons(x, LArt.force a))
        (* move the name to the outside, to catch in a later case *)
        | `Cons(x, `Name(nm,xs)) -> map2 (`Name(nm, `Cons(x,xs)))
        | `Art(a) -> map2 (LArt.force a)
        (* deal with double names from both data *)
        | `Name(nm, `Cons(x, `Art(a))) -> map2 (`Name(nm, `Cons(x, LArt.force a)))
        | `Name(nm1, `Cons(x, `Name(nm2, xs))) ->
          (* should we pair ane fork these names for tracking purposes? *)
          (* let nm1, nm2 = Name.fork @@ Name.pair nm1 nm2 in *)
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 (`Cons(x,xs))))
        | `Name(nm, `Art(a)) -> map2 (`Name(nm, LArt.force a))
        (* after all the double name cases are delt with, handle the default *)
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 xs))
     )
    in
    fun list -> mfn.LArt.mfn_data list

  let name_opt_fork nm =
    match nm with
    | None -> None, None
    | Some nm ->
       let nm1,nm2 = Name.fork nm in
       (Some nm1, Some nm2)

  let name_opt_seq nm1 nm2 =
    match nm1 with
    | Some nm1 -> Some nm1
    | None ->
       ( match nm2 with
         | None -> None
         | Some nm2 -> Some nm2
       )

  (* TODO: simplify this as a special case of rope_reduce_name below *)
  let rec rope_reduce
      ( op_nm : St.name )
      ( op : Elt.t -> Elt.t -> Elt.t )
      : St.Rope.t -> Elt.t option =
    let fnn = Name.pair (Name.gensym "rope_reduce") op_nm in
    let mfn = AEltOption.mk_mfn fnn
      (module St.Rope)
      (fun r rope ->
        let rope_reduce = r.AEltOption.mfn_data in
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
          AEltOption.force (r.AEltOption.mfn_nart nm rope)
        ))
    in
    fun rope -> mfn.AEltOption.mfn_data rope

  let rec rope_reduce_name
    ( op_nm : St.name )
    ( op : Elt.t -> Elt.t -> Elt.t )
    : St.Rope.t -> Elt.t option * name option =
    let fnn = Name.pair (Name.gensym "rope_reduce_name") op_nm in
    let module M =
      ArtLib.MakeArt
        (Name)
        (Types.Tuple2(Types.Option(Elt))(Types.Option(Name)))
    in
    let mfn = M.mk_mfn fnn
    (module Types.Tuple2(St.Rope)(Types.Option(Name)))
    (fun r (rope, nm_opt)->
      let rope_reduce frag = r.M.mfn_data (frag, nm_opt) in
      match rope with
      | `Zero  -> None, nm_opt
      | `One x -> Some x, nm_opt
      | `Two(left,right) ->
        let r1,no1 = rope_reduce left in
        let r2,no2 = rope_reduce right in
        (* find a useful name of the three available *)
        let nm_opt = name_opt_seq nm_opt (name_opt_seq no1 no2) in
        ( match r1, r2 with
        | Some l, Some r -> Some (op l r), nm_opt
        | Some l, None   -> Some l, nm_opt
        | None,   Some r -> Some r, nm_opt
        | None,   None   -> None, nm_opt
        )
      | `Art art -> rope_reduce (St.Rope.Art.force art)
      | `Name (nm1, `Name(nm2, rope)) ->
        let nm = if Name.height nm1 > Name.height nm2 then nm1 else nm2 in
        rope_reduce (`Name(nm, rope))
      | `Name (nm, rope) ->
        let nm1, nm2 = Name.fork nm in
        M.force (r.M.mfn_nart nm1 (rope, Some(nm2)))
    )
    in
    fun rope -> mfn.M.mfn_data (rope, None)

  (* finds the median of a rope in current order, sort first to find true median *)
  let rope_median rope : Elt.t option =
    let len = rope_length rope in
    if len = 0 then None else
    let mid = len/2 in
    rope_nth rope mid

  let list_merge_full
      (compare_nm : name)
      (compare : Elt.t -> Elt.t -> int)
      : name option ->
        name option ->
        St.List.t ->
        St.List.t -> St.List.t =
    let fnn = Name.pair (Name.gensym "list_merge") compare_nm in
    let mfn = LArt.mk_mfn fnn
      (module Types.Tuple4
          (Types.Option(Name))
          (Types.Option(Name))
          (St.List)
          (St.List))
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
        | `Nil, _ -> (match nm_opt2 with None -> list2 | Some nm -> `Name(nm,list2))
        | _, `Nil -> (match nm_opt1 with None -> list1 | Some nm -> `Name(nm,list1))
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
    fun nm1 nm2 l1 l2 -> mfn.LArt.mfn_data (nm1, nm2, l1, l2)

  let list_merge cmp_nm cmp =
    list_merge_full cmp_nm cmp None None

  let rope_mergesort
      ( compare_nm : St.name )
      ( compare : Elt.t -> Elt.t -> int )
      : St.Rope.t -> St.List.t =
    let fnn = Name.pair (Name.gensym "rope_mergesort") compare_nm in
    let merge = list_merge_full compare_nm compare in
    let mfn = St.List.Art.mk_mfn fnn
      (module Types.Tuple2(Types.Option(Name))(St.Rope))
      (fun r (nm,rope) ->
        let rope_mergesort nm rope = r.LArt.mfn_data (nm,rope) in
        ( match rope with
        | `Zero -> `Nil
        | `One x ->
           ( match nm with
             | None -> `Cons(x,`Nil)
             | Some nm -> `Name(nm, `Cons(x, `Nil))
           )
        | `Two(x, y) ->
          (* send the name to the first `Cons *)
          let x_sorted = rope_mergesort nm x in
          let y_sorted = rope_mergesort None y in
          merge None None x_sorted y_sorted

        | `Art art -> rope_mergesort nm (RArt.force art)
        | `Name (nnm, rope) ->
          let nm1,nm2 = Name.fork nnm in
          match nm with
          | None ->
            (* 
              suspend, but don't create a name-seed to here,
              they need to be associated with 'Cons
            *)
            `Art (r.LArt.mfn_nart nm1 (Some nm2,rope))
            (* keep both names active *)
          | Some(nm) ->
            `Name(nm,`Art (r.LArt.mfn_nart nm1 (Some nm2,rope)))
        ))
    in
    fun rope -> mfn.LArt.mfn_data (None,rope)

  let list_to_rope_mergesort
      ( compare_nm : St.name )
      ( compare : Elt.t -> Elt.t -> int )
      : St.List.t -> St.List.t =
    let sort = rope_mergesort compare_nm compare in
    fun list ->
      let rope = rope_of_list list in
      sort rope

end
    
module MakeSeq(ArtLib : ArtLib.S)(Name : Name.S)(Elt : Data.S) =
  SeqWrap(ArtLib)(Name)(Elt)(Make(ArtLib)(Name)(Elt))

(* Makes a key-Value mapping, based on SpreadTrees.
   The mapping is represented as a tree of key-value-sequence pairs.
   Keys are ordered by a comparison function.
   The tree is a binary search tree according to this comparison function.
   Value sequences are stored in an unordered fashion.
*)
module KvMapWrap
  (ArtLib : ArtLib.S)
  (Name : Name.S)
  (Key : Data.S)
  (Val : Data.S)
  (ValSt : S with type  elt = Val.t
              and type name = Name.t) =
struct

  let get_key x = x
  let empty_kv k = k

  module KeySt  = Make(ArtLib)(Name)(Key)
  module KeySeq = MakeSeq       (ArtLib)(Name)(Key)
  module ValSeq = MakeSeq       (ArtLib)(Name)(Val)

  module KeyOptAdpt =
  struct
    type name = Name.t
    module Tmp = Types.Option(Key)
    module Art = ArtLib.MakeArt(Name)(Tmp)
    include Tmp
  end
  module ABool   = ArtLib.MakeArt(Name)(Types.Bool)
  module TArt    = KeySt.Tree.Art
  module LArt    = KeySt.List.Art

  let rec is_bst : Key.t * Key.t  -> KeySt.Tree.t -> bool =
    let mfn = ABool.mk_mfn (Name.gensym "is_bst")
      (module (Types.Tuple3(Key)(Key)(KeySt.Tree)))
      (fun r (lo,hi,tree) ->
        let is_bst (lo,hi) tree = r.ABool.mfn_data (lo,hi,tree) in
        ( match tree with
        | `Leaf `Nil -> true
        | `Leaf `Art art -> is_bst (lo,hi) (`Leaf (KeySt.List.Art.force art))
        | `Leaf `Name (_,rest) -> is_bst (lo,hi) (`Leaf rest)
        | `Leaf `Cons(kv, rest) ->
          Key.compare lo (get_key kv) <= 0
          && Key.compare (get_key kv) hi <= 0
          && KeySeq.list_is_empty rest
        | `Bin(left,kv,right) -> let x = get_key kv in
          ( Key.compare lo x <= 0
            && Key.compare x hi <= 0
            && is_bst (lo, x) left
            && is_bst (x, hi) right )

        | `Art art -> is_bst (lo,hi) (KeySt.Tree.Art.force art)
        | `Name (nm,tree) -> ABool.force (r.ABool.mfn_nart nm (lo,hi,tree))
        ))
    in
    fun (lo,hi) tree -> mfn.ABool.mfn_data (lo,hi,tree)

  let rec list_find
      (list : KeySt.List.t)
      (target : Key.t) : Key.t option =
    ( match list with
    | `Nil -> None
    | `Cons(kv, tl) ->
      if Key.compare (get_key kv) target = 0 then Some kv
      else list_find tl target

    | `Art art -> list_find (KeySt.List.Art.force art) target
    | `Name (_,list) -> list_find list target
    )

  let rec tree_find
      ( tree : KeySt.Tree.t )
      ( target : Key.t ) : Key.t option =
    ( match tree with
    | `Leaf xs -> list_find xs target
    | `Bin(left,kv,right) ->
      let ord = Key.compare target (get_key kv) in
      if ord < 0 then      tree_find left target
      else if ord > 0 then tree_find right target
      else if ord = 0 then Some kv
      else failwith "impossible"

    | `Art art -> tree_find (KeySt.Tree.Art.force art) target
    | `Name (_,tree) -> tree_find tree target
    )

  let rec list_remove : KeySt.List.t -> Key.t -> (Key.t option) * KeySt.List.t =
    let module M = Adapted.ArtTuple2(ArtLib)(Name)(KeyOptAdpt)(KeySt.List) in
    let mfn = M.Art.mk_mfn (Name.gensym "list_remove")
      (module (Types.Tuple2(KeySt.List)(Key)))
      (fun r (list, target) ->
        let list_remove list target = r.M.Art.mfn_data (list, target) in
        ( match list with
        | `Nil -> ( None, `Nil )
        | `Cons(kv, rest) ->
          if Key.compare (get_key kv) target = 0 then
            ( Some kv, rest )
          else
            let res, rem = list_remove rest target in
            (res, `Cons(kv, rem))

        | `Art art -> list_remove (KeySt.List.Art.force art) target
        | `Name (nm, list) ->
          let nm1,nm  = Name.fork nm in
          let nm2,nm3 = Name.fork nm in
          let elt_rem = r.M.Art.mfn_nart nm1 (list, target) in
          let elt,rem = M.split nm2 elt_rem in
          M.Adpt1.Art.force elt, `Name(nm3, `Art rem)
        ))
    in
    fun list target -> mfn.M.Art.mfn_data (list, target)

  let rec tree_remove : KeySt.Tree.t -> Key.t -> (Key.t option) * KeySt.Tree.t =
    let module M = Adapted.ArtTuple2(ArtLib)(Name)(KeyOptAdpt)(KeySt.Tree) in
    let mfn = M.Art.mk_mfn (Name.gensym "tree_remove")
      (module (Types.Tuple2(KeySt.Tree)(Key)))
      (fun r (tree, target) ->
        let tree_remove tree target = r.M.Art.mfn_data (tree, target) in
        ( match tree with
        | `Leaf xs ->
          let res, ys = list_remove xs target in
          (res, `Leaf ys)
        | `Bin(left, kv, right) ->
          let ord = Key.compare target (get_key kv) in
          if ord < 0 then      tree_remove left target
          else if ord > 0 then tree_remove right target
          else if ord = 0 then (Some kv, `Bin(left, empty_kv (get_key kv), right))
          else failwith "impossible"

        | `Art art -> tree_remove (KeySt.Tree.Art.force art) target
        | `Name (nm, tree) ->
          let nm1, nm  = Name.fork nm in
          let nm2, nm3 = Name.fork nm in
          let elt_rem = r.M.Art.mfn_nart nm1 (tree, target) in
          let elt,rem = M.split nm2 elt_rem in
          M.Adpt1.Art.force elt, `Name(nm3, `Art rem)
        ))
    in
    fun tree target -> mfn.M.Art.mfn_data (tree, target)

  let tree_height : KeySt.Tree.t -> int =
    let module M = ArtLib.MakeArt(Name)(Types.Int) in
    let mfn = M.mk_mfn (Name.gensym "tree_height")
      (module KeySt.Tree)
      (fun r tree ->
        let tree_height tree = r.M.mfn_data tree in
        ( match tree with
        | `Leaf xs -> (-1)
        | `Bin(left,x,right) ->
          let hleft = tree_height left in
          let hright = tree_height right in
          1 + (if hleft > hright then hleft else hright)

        | `Art art -> tree_height (KeySt.Tree.Art.force art)
        | `Name (nm, tree) -> M.force (r.M.mfn_nart nm tree)
        ))
    in
    fun tree -> mfn.M.mfn_data tree

  let rec tree_height_diff ( tree : KeySt.Tree.t ) : int =
    ( match tree with
    | `Leaf _ -> 0
    | `Bin(left,x,right) -> (tree_height left) - (tree_height right)
    | `Art art -> tree_height_diff (KeySt.Tree.Art.force art)
    | `Name (_,tree) -> tree_height_diff tree
    )

  let rotate_right : KeySt.Tree.t -> KeySt.Tree.t =
    let mfn = TArt.mk_mfn (Name.gensym "rotate_right")
      (module KeySt.Tree)
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

  let rec rotate_left : KeySt.Tree.t -> KeySt.Tree.t =
    let mfn = TArt.mk_mfn (Name.gensym "rotate_left")
      (module KeySt.Tree)
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

  let nm_tree : Name.t -> KeySt.Tree.t -> KeySt.Tree.t =
    let mfn = TArt.mk_mfn (Name.gensym "nm_tree")
      (module KeySt.Tree)
      (fun r tree -> tree)
    in
    fun nm tree ->
      let nm1,nm2 = Name.fork nm in
      `Name(nm1, `Art (mfn.TArt.mfn_nart nm2 tree))

  let rec avl_insert : Name.t -> KeySt.Tree.t -> Key.t -> KeySt.Tree.t
    =
    let mfn = TArt.mk_mfn (Name.gensym "avl_insert")
      (module (Types.Tuple3(Name)(KeySt.Tree)(Key)))
      (fun r (insert_nm,tree,kv) ->
        let avl_insert nm tree kv = r.TArt.mfn_data (nm,tree,kv) in
        let wrap_avl nm tree =
          let h = tree_height_diff tree in
          assert (h = 0 || h = 1 || h = -1);
          (nm_tree nm tree)
        in
        ( match tree with
        | `Art art -> avl_insert insert_nm (KeySt.Tree.Art.force art) kv
        | `Name(tree_nm, tree) -> avl_insert insert_nm tree kv
        | `Leaf `Nil -> nm_tree insert_nm (`Bin (`Leaf `Nil, kv, `Leaf `Nil))
        | `Leaf _ -> failwith "avl_insert: `Leaf _ : invalid AVL tree"
        | `Bin(left, kv0, right) ->
          let insert_nm1, insert_nm2 = Name.fork insert_nm in
          let ord = Key.compare (get_key kv) (get_key kv0) in
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


  let avl_tree_of_rope : Name.t -> KeySt.Rope.t -> KeySt.Tree.t -> KeySt.Tree.t
    =
    let mfn = TArt.mk_mfn (Name.gensym "avl_tree_of_rope")
      (module Types.Tuple3(Name)(KeySt.Rope)(KeySt.Tree))
      (fun r (nm, rope, tree) ->
        let avl_tree_of_rope nm rope tree = r.TArt.mfn_data (nm, rope, tree) in
        (match rope with
        | `Zero -> tree
        | `One kv -> (*>>>  *) avl_insert nm tree kv
        | `Two (rope1, rope2) ->
          let nm1, nm2 = Name.fork nm in
          let tree' = (avl_tree_of_rope nm1 rope1 tree) in
          avl_tree_of_rope nm2 rope2 tree'
        | `Art(art) -> avl_tree_of_rope nm (KeySt.Rope.Art.force art) tree
        | `Name(nm, rope) ->
          let nm1, nm2 = Name.fork nm in
          TArt.force (r.TArt.mfn_nart nm1 (nm2,rope,tree))
        ))
    in
    fun nm rope tree -> mfn.TArt.mfn_data (nm, rope, tree)

  let avl_tree_of_list : Name.t -> KeySt.List.t -> KeySt.Tree.t -> KeySt.Tree.t
    =
    let mfn = TArt.mk_mfn (Name.gensym "avl_tree_of_list")
      (module Types.Tuple3(Name)(KeySt.List)(KeySt.Tree))
      (fun r (nm, list, tree) ->
        let avl_tree_of_list nm list tree = r.TArt.mfn_data (nm, list, tree) in
        (match list with
        | `Nil -> tree

        | `Cons(x, tl) ->
          let nm1,nm2 = Name.fork nm in
          let tree' = avl_insert nm1 tree x in
          avl_tree_of_list nm2 tl tree'

        | `Name(nm_here, tl) -> avl_tree_of_list nm_here tl tree

        | `Art(art) -> avl_tree_of_list nm (KeySt.List.Art.force art) tree
        ))
    in
    fun nm list tree -> mfn.TArt.mfn_data (nm, list, tree)

end
    
module MakeKvMap(ArtLib : ArtLib.S)(Name : Name.S)(Key : Data.S)(Val : Data.S) =
  KvMapWrap(ArtLib)(Name)(Key)(Val)(Make(ArtLib)(Name)(Val))

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

  module NodeSt = Make (ArtLib) (Name) (NodeData)
  module AdjSt  = Make (ArtLib) (Name) (Adj)
  module EdgeSt = Make (ArtLib) (Name) (Edge)

  module NodeSeq = MakeSeq (NodeSt)
  module AdjSeq  = MakeSeq (AdjSt)
  module EdgeSeq = MakeSeq (EdgeSt)
  module NodeMap = MakeKvMap (ArtLib) (Name) (NodeData) (AdjSt)

  let graph_without_node
      ( graph : NodeMap.KeySt.Tree.data )
      ( node : Nodedata )
      : NodeMap.KeySt.Tree.data * NodeMap.KeySt.data option =
    let node_op, graph = NodeMap.tree_remove graph node in
    ( graph, node_op )

  let tgt_nodes_of_adjs
      ( adjs : AdjSt.List.data )
      : NodeSt.List.data
      = failwith "TODO"

  let rec dfs
      ( graph : NodeMap.KeySt.Tree.data )
      ( stack : NodeSt.List.data )
      : NodeMap.KeySt.List.data
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
      ( graph : NodeMap.KeySt.Tree.data )
      ( queue : NodeSt.Tree.data )
      : NodeMap.KeySt.List.data
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
              `Art( NodeMap.KeySt.List.Art.cell (
              dfs graph (NodeSt.List.Art.force art) ) ) *)
    )
end
*)
(*
module ExprLang = struct

  module type ExprLangType = sig
    module ArtLib : ArtLib.S
    module Name : Name.S
    module Value : Data.S

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
    | `Name of name * 'art art_expr
    ]
    module rec Expr : sig
      module Data : Data.S
      module Art  : Art.S
    end
      with type data = Expr.Art.t art_expr
      and type Art.data = Expr.Art.t art_expr
      and type Art.name = name
  end

  module Make
    (ArtLib : ArtLib.S)
    (Name   : Name.S)
    (Value  : Data.S) : ExprLangType = struct
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
      | `Name of name * 'art art_expr
      ]

      module rec Expr : sig
        module Data : Data.S
        module Art : Art.S
      end
        with type data = Expr.Art.t art_expr
        and type Art.data = Expr.Art.t art_expr
        and type Art.name = name
              =
      struct
        module Data = struct
          type t = Expr.Art.t art_expr

          let rec show exp =
            ( match exp with
            | `Value v -> Value.show v
            | `Binop ((s,_), e1, e2) -> "Binop("^s^","^show e1^","^show e2^")"
            | `Uniop ((s,_), e) -> "Uniop("^s^","^show e^")"
            | `Let (v,e1,e2) -> "Let("^v^","^show e1^","^show e2^")"
            | `Var v -> "Var("^v^")"
            | `Art a -> "Art("^Expr.Art.show a^")"
            | `Name(nm, e) -> "Name("^Name.show nm^","^show e^")"
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

    module Values = Make(ArtLib)(Name)(Value)
    module Env = MakeKvMap(ArtLib)(Name)(Types.String)(Values)

    type env = Env.KeySt.Tree.t
    type expr = Expr.data
    type value = Value.t
    type nm = name

    let eval_big : nm -> env -> expr -> value option
      =
      let mfn = VOptionArt.mk_mfn (Name.gensym "eval_big")
        (module Types.Tuple3(Name)(Env.KeySt.Tree.Data)(Expr.Data))
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
        (module Types.Tuple3(Name)(Env.KeySt.Tree.Data)(Expr.Data))
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
*)
