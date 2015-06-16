(* use Adapton *)
open Adapton_lib

(* setup adapton types *)
module N = Adapton.Name
module T = Adapton.Types
module Point = T.Tuple2(T.Int)(T.Int)

(* setup adapton list for snake body *)
open Adapton.Structures.SpreadTree
module PointST = MakeSpreadTree(Adapton.NameLib)(N)(Point)
module PointSeq = MakeSeq(PointST)

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

(* setup streams *)
let rec rabbits = 
  { update = (fun _ -> ());
    data = r_initial;
    collect = fun () ->
      let r = rabbits.data in
      let f = foxes.data in
      let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
      let newr = r + (r * (r_birthrate - r_deathrate * r / r_high_population) / 100) - (predation) in
      fun s ->
        s.data <- newr
  }
and foxes =
  { update = (fun _ -> ());
    data = f_initial;
    collect = fun () ->
      let r = rabbits.data in
      let f = foxes.data in
      let predation = min (f * hunting_max / 100) (r - (r * survival / 100)) in
      let extra_food = predation * nutrition / 100 in
      let newf = f + (f * (f_birthrate - f_deathrate * f / f_high_population) / 100) + extra_food in
      fun s -> s.data <- newf
  }
and output =
  { update = (fun _ -> ());
    data = 0;
    collect = fun () ->
      let r = rabbits.data in
      let f = foxes.data in
      Printf.printf "Rabbits: %d, Foxes: %d\n" r f;
      fun _ -> ()
  }
and tick_counter =
  { update = (fun _ -> ());
    data = 0;
    collect = fun () ->
      let v = if tick_counter.data > 20 then exit 0 else tick_counter.data + 1 in
      fun s -> s.data <- v
  }
and streams = [ rabbits; foxes; output; tick_counter ]

(* go! *)
let _ = update_loop streams
