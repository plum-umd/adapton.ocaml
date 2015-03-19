(*
 -- TODO** : Test this code for correctness.
 -- TODO   : Maybe make it use OCaml's Rational Number representation instead of IEEE floats.
 -- TODO** : Use Nominal Adapton's List representation instead of OCaml's built-in list rep
*)

(*
  2D geometry primitives, adapted from here:
  https://github.com/matthewhammer/ceal/blob/master/src/apps/common/geom2d.c
*)

(* this is in development and will not compile *)

open Adapton_core
open Primitives

module Types = AdaptonTypes

type point  = float * float
type line   = point * point

let point_sub : point -> point -> point =
  fun p q -> (fst p -. fst q, snd p -. snd q)
                 
let points_distance : point -> point -> float =
  fun p q ->
  sqrt ( (+.)
           (((fst p) -. (fst q)) *. ((fst p) -. (fst q)))
           (((snd p) -. (snd q)) *. ((snd p) -. (snd q)))
       )

let magnitude : point -> float =
  fun p -> sqrt ((fst p) *. (fst p)) +. ((snd p) *. snd p)

let cross_product : point -> point -> float =
  (* (p->x * q->y) - (p->y * q->x) *)
  fun p q -> ((fst p) *. (snd q)) -. ((snd p) *. (fst q))

let line_point_distance : line -> point -> float =
  fun line point ->
  let diff1 = point_sub (snd line) (fst line) in
  let diff2 = point_sub (fst line) point in
  let diff3 = point_sub (snd line) (fst line) in
  let numer = abs_float (cross_product diff1 diff2) in
  let denom = magnitude diff3 in
  numer /. denom

  let line_side_test : line -> point -> bool =
    fun line p ->
    if (fst line) = p || (snd line) = p then
      (* Invariant: to ensure that quickhull terminates, we need to
         return false for the case where the point equals one of the
         two end-points of the given line. *)
      false
    else
      let diff1 = point_sub (snd line) (fst line) in
      let diff2 = point_sub (fst line) p in
      let cross = cross_product diff1 diff2 in
      if cross <= 0.0 then false else true

module StMake (IntsSt : SpreadTree.SpreadTreeType 
  with type Data.t = Types.Int.t
)
= struct

  module Name = IntsSt.Name
  module ArtLib = IntsSt.ArtLib

  (* type points = point list *)
  module PointsSt = SpreadTree.MakeSpreadTree
    (ArtLib)(Name)(Types.Tuple2(Types.Float)(Types.Float))
  module PList = PointsSt.Rope
  module AccumList = PointsSt.List
  module Seq = SpreadTree.MakeSeq(PointsSt)
  module Point = PointsSt.Data

  (* modified from Spreadtree list_map_paired to convert between data types *)
  let points_of_ints
    : IntsSt.List.Data.t -> PointsSt.List.Data.t =
    let module LArt = IntsSt.List.Art in
    let module PArt = PointsSt.List.Art in
    let op a b = (float_of_int a, float_of_int b) in
    let mfn = PArt.mk_mfn (Name.gensym "points_of_ints")
      (module IntsSt.List.Data)
      (fun r list ->
        let map2 = r.PArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(_, `Nil) -> `Nil
        | `Cons(x, `Cons(y, ys)) -> `Cons(op x y, map2 ys)
        | `Cons(x, `Art(a)) -> map2 (`Cons(x, LArt.force a))
        | `Cons(x, `Name(nm,xs)) -> map2 (`Name(nm, `Cons(x,xs)))
        | `Art(a) -> map2 (LArt.force a)
        | `Name(nm, `Cons(x, `Art(a))) -> map2 (`Name(nm, `Cons(x, LArt.force a)))
        | `Name(nm1, `Cons(x, `Name(nm2, xs))) ->
          `Name(nm1, `Art(r.PArt.mfn_nart nm2 (`Cons(x,xs))))
        | `Name(nm, `Art(a)) -> map2 (`Name(nm, LArt.force a))
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.PArt.mfn_nart nm2 xs))
     )
    in
    fun list -> mfn.PArt.mfn_data list

  (* modified from SpreadTree list_map to convert between data types *)
  let ints_of_points
    : PointsSt.List.Data.t -> IntsSt.List.Data.t = 
    let module LArt = IntsSt.List.Art in
    let module PArt = PointsSt.List.Art in
    let mfn = LArt.mk_mfn (Name.gensym "ints_of_points")
      (module PointsSt.List.Data)
      (fun r list -> 
        let list_map = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons((x,y), xs) -> `Cons(int_of_float x, `Cons(int_of_float y, list_map xs))
        | `Art(a) -> list_map (PArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 xs))
      )
    in
    fun list -> mfn.LArt.mfn_data list

  let points_of_int_list : IntsSt.List.Data.t -> PList.Data.t =
  fun inp ->
    let pointslist = points_of_ints inp in
    Seq.rope_of_list pointslist

  let int_list_of_points : PList.Data.t -> IntsSt.List.Data.t =
  fun inp ->
    let pointslist = Seq.list_of_rope inp `Nil in
    ints_of_points pointslist

  let furthest_point_from_line : line -> PList.Data.t -> Point.t * Name.t =
    (* Used in the "pivot step".  the furthest point defines the two
       lines that we use for the "filter step".
       Note: To make this into an efficient IC algorithm, need to use a
       balanced reduction.  E.g., using either a rope reduction, or an
       iterative list reduction.  *)
    fun (l1,l2) points ->
      let max_point pt1 pt2 = 
        let d1 = line_point_distance (l1, l2) pt1 in
        let d2 = line_point_distance (l1, l2) pt2 in
        if d1 > d2 then pt1 else pt2
      in
      let reduce = Seq.rope_reduce_name (Name.gensym "fpfl") max_point in
      match reduce points with
      | None, _ -> failwith "no points"
      | _, None -> failwith "no name"
      | Some(x), Some(nm) -> x, nm


  let quickhull_rec : line -> PList.Data.t -> AccumList.Data.t -> AccumList.Data.t =
    (* Adapton: Use a memo table here.  Our accumulator, hull_accum, is
       a nominal list.  We need to use names because otherwise, the
       accumulator will be unlikely to match after a small change. *)
    let module AA = AccumList.Art in
    let mfn = AA.mk_mfn (Name.gensym "quick_hull")
      (module Types.Tuple3
        (Types.Tuple2(Point)(Point))
        (PList.Data)
        (AccumList.Data)
      )
      (fun r ((p1,p2) as line, points, hull_accum) ->
        let quickhull l p h = r.AA.mfn_data (l,p,h) in
        let filter = Seq.rope_filter (Name.gensym "side_of_line") (line_side_test line)in
        match points with
        | `Zero -> hull_accum
        | `Art(a) -> quickhull line (PList.Art.force a) hull_accum
        | `Name(_, ps) -> quickhull line ps hull_accum
        | _ ->
          let points_above_line = filter points in
          let pivot_point, p_nm = furthest_point_from_line line points in
          let nm1, nms = Name.fork p_nm in
          let nm2, nms = Name.fork nms in
          let nm3, nm4 = Name.fork nms in
          let l_line = (p1, pivot_point) in
          let r_line = (pivot_point, p2) in
          (* two lazy recursive steps *)
          let hull_accum = `Cons(pivot_point,
            `Name(nm1, `Art(r.AA.mfn_nart nm2 (r_line, points_above_line, hull_accum)))
          ) in
          let hull_accum = `Cons(pivot_point,
            `Name(nm3, `Art(r.AA.mfn_nart nm4 (l_line, points_above_line, hull_accum)))
          ) in
          hull_accum
      ) in
    fun l p h -> mfn.AA.mfn_data (l,p,h)

  (* 
    for this fun, we need to:
    take an Int St.List, reduce it to a Point St.Rope,
    apply the algorithm, convert it back to an Int St.List

    This should probably be done in two steps, one interfacing
    with experiments.ml and the other as the main routine
  *)
(*   let quickhull : points -> points =
    (* A convex hull consists of an upper and lower hull, each computed
       recursively using quickhull_rec.  We distinguish these two
       sub-hulls using an initial line that is defined by the points
       with the max and min X value. *)
    fun points ->
    let p_min_x = List.fold_left (fun p q -> if (fst p) < (fst q) then p else q) (max_float, 0.0) points in
    let p_max_x = List.fold_left (fun p q -> if (fst p) > (fst q) then p else q) (min_float, 0.0) points in
    let hull = quickhull_rec (p_min_x, p_max_x) [p_max_x] points in
    let hull = quickhull_rec (p_max_x, p_min_x) (p_min_x :: hull) points in
    hull
 *)
end