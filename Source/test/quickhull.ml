(*
 -- TODO** : Test this code for correctness.
 -- TODO   : Maybe make it use OCaml's Rational Number representation instead of IEEE floats.
*)

(*
  2D geometry primitives, adapted from here:
  https://github.com/matthewhammer/ceal/blob/master/src/apps/common/geom2d.c
*)

open Adapton_core
open Primitives

module Types = AdaptonTypes



(* ////////////////// *)
(* // Common Setup // *)
(* ////////////////// *)

type point  = float * float
type line   = point * point
type points = point list

(* breaks an int in to a pair of floats *)
(* takes lower 8 bits and next 8 bits as ints, converts to floats *)
let point_of_int i =
  let x = float_of_int (i land 255) in
  let y = float_of_int ((i lsr 8) land 255) in
  (x, y)
let int_of_point (x,y) =
  let x_bits = ((int_of_float x) land 255) in
  let y_bits = ((int_of_float y) land 255) lsl 8 in
  x_bits lor y_bits

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

let x_max p q =
  if (fst p) > (fst q) then p else q
let y_max p q =
  if (snd p) > (snd q) then p else q
let x_min p q =
  if (fst p) < (fst q) then p else q
let y_min p q =
  if (snd p) < (snd q) then p else q

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

let max_point_from_line line pt1 pt2 = 
  let d1 = line_point_distance line pt1 in
  let d2 = line_point_distance line pt2 in
  if d1 > d2 then pt1 else pt2

let furthest_point_from_line : line -> points -> (point * float) =
  (* Used in the "pivot step".  the furthest point defines the two
   lines that we use for the "filter step".

   Note: To make this into an efficient IC algorithm, need to use a
   balanced reduction.  E.g., using either a rope reduction, or an
   iterative list reduction.  *)
  fun line points ->
  match points with
  | [] -> failwith "no points"
  | p::points ->
     List.fold_left
       (fun (q,max_dis) p ->
        let d = line_point_distance line p in
        if d > max_dis then p, d else q, max_dis
       )
       (p,line_point_distance line p)
       points


(* ///////////////////////////// *)
(* // Non-Incremental version // *)
(* ///////////////////////////// *)

let rec quickhull_rec : line -> points -> points -> points =
  (* Adapton: Use a memo table here.  Our accumulator, hull_accum, is
   a nominal list.  We need to use names because otherwise, the
   accumulator will be unlikely to match after a small change. *)

  (* INVARIANT: All the input points are *above* the given line. *)
  fun line points hull_accum ->
  match points with
  | [] -> hull_accum
  | _ ->
     let pivot_point, _ = furthest_point_from_line line points in
     let l_line = (fst line, pivot_point) in
     let r_line = (pivot_point, snd line) in

     (* Avoid DCG Inconsistency: *)
     (* Use *two different* memo tables ('namespaces') here, since we process the same list twice! *)
     let l_points = List.filter (line_side_test l_line) points in
     let r_points = List.filter (line_side_test r_line) points in

     let hull_accum = quickhull_rec r_line r_points hull_accum in
     quickhull_rec l_line l_points (pivot_point :: hull_accum)

let quickhull : points -> points =
  (* A convex hull consists of an upper and lower hull, each computed
   recursively using quickhull_rec.  We distinguish these two
   sub-hulls using an initial line that is defined by the points
   with the max and min X value. *)
  fun points ->
  let p_min_x = List.fold_left (fun p q -> if (fst p) < (fst q) then p else q) (max_float, 0.0) points in
  let p_max_x = List.fold_left (fun p q -> if (fst p) > (fst q) then p else q) (min_float, 0.0) points in
  let line_above = (p_min_x, p_max_x) in
  let line_below = (p_max_x, p_min_x) in (* "below" here means swapped coordinates from "above". *)
  let points_above = List.filter (line_side_test line_above) points in
  let points_below = List.filter (line_side_test line_below) points in
  let hull = quickhull_rec line_above points_above [p_max_x] in
  let hull = quickhull_rec line_below points_below (p_min_x::hull) in
  hull

let list_quickhull : int list -> int list = fun inp ->
  let points = List.map point_of_int inp in
  let hull = quickhull points in
  List.map int_of_point hull



(* ///////////////////////// *)
(* // Incremental version // *)
(* ///////////////////////// *)

(* creates an incremental version of quickhull based on a SpreadTree integer list *)
module StMake (IntsSt : SpreadTree.SpreadTreeType 
  with type Data.t = Types.Int.t
)
= struct

  module Name = IntsSt.Name
  module ArtLib = IntsSt.ArtLib

  (* type points = point list *)
  module PointsSt = SpreadTree.MakeSpreadTree
    (ArtLib)(Name)(Types.Tuple2(Types.Float)(Types.Float))
  module PointRope = PointsSt.Rope
  module AccumList = PointsSt.List
  module Seq = SpreadTree.MakeSeq(PointsSt)
  module Point = PointsSt.Data

  (* Hack to fix AccumList.Art.cell, etc *)
  let accum_cell =
    let fakecell = AccumList.Art.mk_mfn (Name.gensym "Accumulator_cells")
      (module AccumList.Data) (fun r data -> data)
    in
    fun nm data -> (fakecell.AccumList.Art.mfn_nart nm data)
  let points_cell =
    let fakecell = PointRope.Art.mk_mfn (Name.gensym "PointRope_cells")
      (module PointRope.Data) (fun r data -> data)
    in
    fun nm data -> (fakecell.PointRope.Art.mfn_nart nm data)

  (* modified from SpreadTree list_map to convert between data types *)
  let points_of_ints
    : IntsSt.List.Data.t -> PointsSt.List.Data.t =
    let module LArt = IntsSt.List.Art in
    let module PArt = PointsSt.List.Art in
    let mfn = PArt.mk_mfn (Name.gensym "points_of_ints")
      (module IntsSt.List.Data)
      (fun r list -> 
        let list_map = r.PArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(x, xs) -> `Cons(point_of_int x, list_map xs)
        | `Art(a) -> list_map (LArt.force a)
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
        | `Cons(x, xs) -> `Cons(int_of_point x, list_map xs)
        | `Art(a) -> list_map (PArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 xs))
      )
    in
    fun list -> mfn.LArt.mfn_data list

  let points_rope_of_int_list : IntsSt.List.Data.t -> PointRope.Data.t =
  fun inp ->
    let pointslist = points_of_ints inp in
    Seq.rope_of_list pointslist

  let int_list_of_points_rope : PointRope.Data.t -> IntsSt.List.Data.t =
  fun inp ->
    let pointslist = Seq.list_of_rope inp `Nil in
    ints_of_points pointslist

  (* modified from SpreadTree rope_filter to internalize above_line *)
  (* TODO: optimize, compact zeros *)
  let above_line : line -> PointRope.Data.t -> PointRope.Data.t = 
    let fnn = (Name.gensym "above_line") in
    let mfn = PointRope.Art.mk_mfn fnn
      (module Types.Tuple2(Types.Tuple2(Point)(Point))(PointRope.Data))
      (fun r (line, pts) ->
        let above_line l = r.PointRope.Art.mfn_data (line, l) in
        match pts with
        | `Zero -> `Zero
        | `One(x) -> if (line_side_test line x) then `One(x) else `Zero
        | `Two(x,y) -> `Two(above_line x, above_line y)
        | `Art(a) -> above_line (PointRope.Art.force a)
        | `Name(nm, pts) ->
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.PointRope.Art.mfn_nart nm2 (line,pts)))
      )
    in
    fun line pts -> mfn.PointRope.Art.mfn_data (line, pts)

  (* modified from SpreadTree rope_reduce_name to internalize furthest_from_line *)
  let rec find_furthest
    : line -> PointRope.Data.t -> Point.t option * Name.t option =
    let opt_seq nm1 nm2 =
      match nm1 with
      | Some nm1 -> Some nm1
      | None ->
        match nm2 with
        | None -> None
        | Some nm2 -> Some nm2
    in
    let max_point = max_point_from_line in
    let fnn = (Name.gensym "find_furthest")  in
    let module M = ArtLib.MakeArt(Name)(Types.Tuple2
      (Types.Option(Point))(Types.Option(Name))
    ) in
    let mfn = M.mk_mfn fnn
    (module Types.Tuple3(Types.Tuple2(Point)(Point))(PointRope.Data)(Types.Option(Name)))
    (fun r (line, points, nm_opt)->
      let furthest frag = r.M.mfn_data (line, frag, nm_opt) in
      match points with
      | `Zero  -> None, nm_opt
      | `One x -> Some x, nm_opt
      | `Two(left,right) ->
        let p1,no1 = furthest left in
        let p2,no2 = furthest right in
        (* find a useful name of the three available *)
        let nm_opt = opt_seq nm_opt (opt_seq no1 no2) in
        ( match p1, p2 with
        | Some l, Some r -> Some (max_point line l r), nm_opt
        | Some l, None   -> Some l, nm_opt
        | None,   Some r -> Some r, nm_opt
        | None,   None   -> None, nm_opt
        )
      | `Art art -> furthest (PointRope.Art.force art)
      | `Name (nm1, `Name(nm2, pts)) ->
        let nm = if Name.height nm1 > Name.height nm2 then nm1 else nm2 in
        furthest (`Name(nm, pts))
      | `Name (nm, pts) ->
        let nm1, nm2 = Name.fork nm in
        M.force (r.M.mfn_nart nm1 (line, pts, Some(nm2)))
    )
    in
    fun ln pts -> mfn.M.mfn_data (ln, pts, None)

  let furthest_point_from_line : line -> PointRope.Data.t -> Point.t * Name.t =
    (* Used in the "pivot step".  the furthest point defines the two
       lines that we use for the "filter step".
       Note: To make this into an efficient IC algorithm, need to use a
       balanced reduction.  E.g., using either a rope reduction, or an
       iterative list reduction.  *)
    fun line points ->
      match find_furthest line points with
      | None, _ -> failwith "no points far from line"
      | _, None -> failwith "no name"
      | Some(x), Some(nm) -> x, nm

  (* ////////////////// *)
  (* // List Version // *)
  (* ////////////////// *)

  let quickhull_rec : Name.t -> line -> PointRope.Data.t -> AccumList.Data.t -> AccumList.Data.t =
    (* Adapton: Use a memo table here.  Our accumulator, hull_accum, is
       a nominal list.  We need to use names because otherwise, the
       accumulator will be unlikely to match after a small change. *)
    fun (namespace : Name.t) ->
    let module AA = AccumList.Art in
    let mfn = AA.mk_mfn (Name.pair (Name.gensym "quick_hull") namespace)
      (module Types.Tuple3
        (Types.Tuple2(Point)(Point))
        (PointRope.Data)
        (AccumList.Data)
      )
      (* INVARIANT: All the input points are *above* the given line. *)
      (fun r ((p1,p2) as line, points, hull_accum) ->
        (* using length because rope_filter is not currently guarenteed to be minimal, ei, might be `Two(`Zero, One(x)) *)
        if Seq.rope_length points <= 0 then hull_accum else
        let pivot_point, p_nm = furthest_point_from_line line points in
        let l_line = (p1, pivot_point) in
        let r_line = (pivot_point, p2) in
        let l_points = above_line l_line points in
        let r_points = above_line r_line points in
        let nms = p_nm in
          let nm1, nms = Name.fork nms in
          let nm2, nms = Name.fork nms in
          let nm3, nms = Name.fork nms in
          let nm0 = nms in
        let hull_accum = `Cons(pivot_point,
          `Name(nm0, `Art(r.AA.mfn_nart nm1 (r_line, r_points, hull_accum))))
        in
        let hull_accum = 
          `Name(nm2, `Art(r.AA.mfn_nart nm3 (l_line, l_points, hull_accum)))
        in
        hull_accum
      )
    in
    fun l p h -> mfn.AA.mfn_data (l,p,h)

  let quickhull : PointRope.Data.t -> AccumList.Data.t =
    (* Allocate these two memoized tables *statically* *)
    let qh_upper = quickhull_rec (Name.gensym "upper") in
    let qh_lower = quickhull_rec (Name.gensym "lower") in
    (* A convex hull consists of an upper and lower hull, each computed
       recursively using quickhull_rec.  We distinguish these two
       sub-hulls using an initial line that is defined by the points
       with the max and min X value. *)
    fun points ->
      let min = Seq.rope_reduce (Name.gensym "points_min") x_min in
      let max = Seq.rope_reduce (Name.gensym "points_max") x_max in
      let p_min_x = match min points with None -> failwith "no points min_x" | Some(x) -> x in
      let p_max_x = match max points with None -> failwith "no points min_y" | Some(x) -> x in
      let line_above = (p_min_x, p_max_x) in
      let line_below = (p_max_x, p_min_x) in (* "below" here means swapped coordinates from "above". *)
      let points_above = above_line line_above points in
      let points_below = above_line line_below points in
      let nms = Name.nondet() in
      let nm1, nms = Name.fork nms in
      let nm2, nms = Name.fork nms in
      let nm3, nm4 = Name.fork nms in
      (* using create-force to branch off subcomputations here *)
      (* this helps a lot *)
      let hull = AccumList.Art.force (accum_cell nm1 (
        qh_upper line_above points_above (`Name(nm2, `Cons(p_max_x, `Nil)))
      )) in
      let hull = AccumList.Art.force (accum_cell nm3 (
        qh_lower line_below points_below (`Name(nm4, `Cons(p_min_x, hull)))
      )) in
      hull

  let list_quickhull : IntsSt.List.Data.t -> IntsSt.List.Data.t =
  fun list ->
    let points = points_rope_of_int_list list in
    let hull = quickhull points in
    ints_of_points hull

  (* ////////////////// *)
  (* // Rope Version // *)
  (* ////////////////// *)

  let rope_quickhull_rec : Name.t -> line -> PointRope.Data.t -> PointRope.Data.t =
    (* Adapton: Use a memo table here.  Our accumulator, hull_accum, is
       a nominal list.  We need to use names because otherwise, the
       accumulator will be unlikely to match after a small change. *)
    fun (namespace : Name.t) ->
    let module AA = PointRope.Art in
    let mfn = AA.mk_mfn (Name.pair (Name.gensym "rope_quick_hull") namespace)
      (module Types.Tuple2
        (Types.Tuple2(Point)(Point))
        (PointRope.Data)
      )
      (* INVARIANT: All the input points are *above* the given line. *)
      (fun r ((p1,p2) as line, points) ->
        (* using length because rope_filter is not currently guarenteed to be minimal, ei, might be `Two(`Zero, One(x)) *)
        if Seq.rope_length points <= 0 then `Zero else
        let pivot_point, p_nm = furthest_point_from_line line points in
        let l_line = (p1, pivot_point) in
        let r_line = (pivot_point, p2) in
        let l_points = above_line l_line points in
        let r_points = above_line r_line points in
        let nms = p_nm in
          let nm1, nms = Name.fork nms in
          let nm2, nms = Name.fork nms in
          let nm3, nms = Name.fork nms in
          let nm0 = nms in
        `Two(`One(pivot_point),`Two(
          `Name(nm0, `Art(r.AA.mfn_nart nm1 (r_line, r_points))),
          `Name(nm2, `Art(r.AA.mfn_nart nm3 (l_line, l_points)))
        ))
      )
    in
    fun l p -> mfn.AA.mfn_data (l,p)

  let rope_quickhull_main : PointRope.Data.t -> PointRope.Data.t =
    (* Allocate these two memoized tables *statically* *)
    let qh_upper = rope_quickhull_rec (Name.gensym "upper") in
    let qh_lower = rope_quickhull_rec (Name.gensym "lower") in
    (* A convex hull consists of an upper and lower hull, each computed
       recursively using quickhull_rec.  We distinguish these two
       sub-hulls using an initial line that is defined by the points
       with the max and min X value. *)
    fun points ->
      let min = Seq.rope_reduce (Name.gensym "points_min") x_min in
      let max = Seq.rope_reduce (Name.gensym "points_max") x_max in
      let p_min_x = match min points with None -> failwith "no points min_x" | Some(x) -> x in
      let p_max_x = match max points with None -> failwith "no points min_y" | Some(x) -> x in
      let line_above = (p_min_x, p_max_x) in
      let line_below = (p_max_x, p_min_x) in (* "below" here means swapped coordinates from "above". *)
      let points_above = above_line line_above points in
      let points_below = above_line line_below points in
      let nms = Name.nondet() in
        let nm1, nms = Name.fork nms in
        let nm2, nms = Name.fork nms in
        let nm3, nms = Name.fork nms in
        let nm0 = nms in
      `Two(`One(p_max_x),`Two(
        `Name(nm0, `Art(points_cell nm1 (qh_upper line_above points_above))),
        `Two(`One(p_min_x),
        `Name(nm2, `Art(points_cell nm3 (qh_lower line_below points_below)))
      )))

  let rope_quickhull : IntsSt.List.Data.t -> IntsSt.List.Data.t =
  fun list ->
    let points = points_rope_of_int_list list in
    let hull_rope = rope_quickhull_main points in
    int_list_of_points_rope hull_rope


end
