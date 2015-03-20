(*
 -- TODO** : Test this code for correctness.
 -- TODO   : Maybe make it use OCaml's Rational Number representation instead of IEEE floats.
 -- TODO** : Use Nominal Adapton's List representation instead of OCaml's built-in list rep
 *)

(*
 2D geometry primitives, adapted from here:
 https://github.com/matthewhammer/ceal/blob/master/src/apps/common/geom2d.c
 *)
type point  = float * float
type line   = point * point
type points = point list

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

let rec quickhull_rec : line -> points -> points -> points =
  (* Adapton: Use a memo table here.  Our accumulator, hull_accum, is
   a nominal list.  We need to use names because otherwise, the
   accumulator will be unlikely to match after a small change. *)

  (* INVARIANT: All the input points are *above* the given line. *)
  fun line points hull_accum ->
  match points with
  | [] -> []
  | _ ->
     let pivot_point, _ = furthest_point_from_line line points in
     let l_line = (fst line, pivot_point) in
     let r_line = (pivot_point, snd line) in

     (* Avoid DCG Inconsistency: *)
     (* Use *two different* memo tables ('namespaces') here, since we process the same list twice! *)
     let l_points = List.filter (line_side_test l_line) points in
     let r_points = List.filter (line_side_test r_line) points in

     let hull_accum = quickhull_rec l_line l_points hull_accum in
     quickhull_rec r_line r_points (pivot_point :: hull_accum)

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
  let hull = quickhull_rec line_above [p_max_x] points_above in
  let hull = quickhull_rec line_below hull points_below in
  p_min_x :: hull
