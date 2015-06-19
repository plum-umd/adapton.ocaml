(* use Adapton *)
open Adapton_lib

(* setup adapton types *)
module N = Adapton.Name
module T = Adapton.Types
module Point = T.Tuple2(T.Int)(T.Int)

(* setup adapton list for no reason *)
open Adapton.Structures.SpreadTree
module PointST = MakeSpreadTree(Adapton.NameLib)(N)(Point)
module PointSeq = MakeSeq(PointST)

(* setup adapton for simple temp sim *)
module AInt = Adapton.NameLib.MakeArt(N)(T.Int)

(* define streams *)
type 'a stream = {
  mutable data : 'a;                     (* stream's current value *)
  collect : unit -> ('a stream -> unit); (* function to gather inputs *)
  mutable update : 'a stream -> unit     (* function to modify current value *)
}

(* stream update loop *)
let gatherdata s = s.update <- s.collect()
let mutatedata s = s.update s
let update_loop = fun streams ->
  let rec loop = fun () ->
    List.iter gatherdata streams;
    List.iter mutatedata streams;
    loop()
  in
  loop()

(* simulation parameters *)
let hunting_max = 500
let survival = 70
let nutrition = 10
let r_initial = 50
let r_birthrate = 50
let r_deathrate = 10
let r_high_population = 1000
let f_initial = 2
let f_birthrate = 10
let f_deathrate = 30
let f_high_population = 200

let rabbit_updater =
  let memo = AInt.mk_mfn (N.gensym "rabbit updater")
    (module T.Tuple2(AInt)(AInt))
    (fun _ (rs, fs) ->
      let r = AInt.force rs in
      let f = AInt.force fs in
      let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
      let newr = r + (r * (r_birthrate - r_deathrate * r / r_high_population) / 100) - (predation) in
      newr
    )
    in
    fun rs fs -> memo.AInt.mfn_data (rs,fs)

(* same as above, and same non-adapton version, see note in comment below *)
let fox_updater rs fs =
    let r = AInt.force rs.data in
    let f = AInt.force fs.data in
    let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
    let extra_food = predation * nutrition / 100 in
    let newf = f + (f * (f_birthrate - f_deathrate * f / f_high_population) / 100) + extra_food in
    newf

(* setup streams *)
let rec rabbits = {
  update = (fun _ -> ());
  data = AInt.cell (N.nondet()) r_initial;
  collect = fun () ->
    let r = AInt.force rabbits.data in
    let f = AInt.force foxes.data in
    let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
    let newr = r + (r * (r_birthrate - r_deathrate * r / r_high_population) / 100) - (predation) in
    fun s ->
      AInt.set s.data newr
}
and a_rabbits = {
  update = (fun _ -> ());
  data = AInt.cell (N.nondet()) r_initial;
  collect = fun () ->
    let newr = rabbit_updater a_rabbits.data a_foxes.data in
    fun s -> AInt.set s.data newr
}
and foxes = {
  update = (fun _ -> ());
  data = AInt.cell (N.nondet()) f_initial;
  collect = fun () ->
    let r = AInt.force rabbits.data in
    let f = AInt.force foxes.data in
    let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
    let extra_food = predation * nutrition / 100 in
    let newf = f + (f * (f_birthrate - f_deathrate * f / f_high_population) / 100) + extra_food in
    fun s ->
      AInt.set s.data newf
}
(* this kind of expression is not allowed on the right side of 'let rec': recursive value as param to function call
and fox_updater = AInt.thunk (N.nondet()) (fun () ->
  let r = AInt.force a_rabbits.data in
  let f = AInt.force a_foxes.data in
  let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
  let extra_food = predation * nutrition / 100 in
  let newf = f + (f * (f_birthrate - f_deathrate * f / f_high_population) / 100) + extra_food in
  newf
)
*)
and a_foxes = {
  update = (fun _ -> ());
  data = AInt.cell (N.nondet()) f_initial;
  collect = fun () ->
    let newf = fox_updater a_rabbits a_foxes in
    fun s -> AInt.set s.data newf
}
and output = {
  update = (fun _ -> ());
  data = AInt.cell (N.nondet()) 0;
  collect = fun () ->
    let r = AInt.force rabbits.data in
    let f = AInt.force foxes.data in
    let ar = AInt.force rabbits.data in
    let af = AInt.force foxes.data in
    Printf.printf "Rabbits: %d|%d, Foxes: %d|%d\n" r ar f af;
    fun _ -> ()
}
and tick_counter = {
  update = (fun _ -> ());
  data = AInt.cell (N.nondet()) 0;
  collect = fun () ->
    let v = if AInt.force tick_counter.data > 20 then exit 0 else AInt.force tick_counter.data + 1 in
    fun s -> AInt.set s.data v
}
and streams = [ rabbits; foxes; output; tick_counter ]

(* go! *)
let _ = update_loop streams
