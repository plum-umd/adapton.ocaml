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
    Array.iter gatherdata streams;
    Array.iter mutatedata streams;
    loop()
  in
  loop()

(* setup streams *)
let rec s1 = { update = (fun _ -> ());
  data = 0;
  collect = fun () ->
    let v = s1.data + 1 in
    fun s ->
      s.data <- v
}
and s2 = { update = (fun _ -> ());
  data = -35;
  collect = fun () ->
    let v = (s1.data - 2) * 4 in
    Printf.printf "%d\n" v; 
    fun s -> s.data <- v
}
and exit_flag = { update = (fun _ -> ());
  data = 0;
  collect = fun () ->
    let v = if s1.data > 10 then exit 1 else 0 in
    fun s -> s.data <- v
}
let streams = [| s1; s2; exit_flag |]

(* go! *)
let _ = update_loop streams
