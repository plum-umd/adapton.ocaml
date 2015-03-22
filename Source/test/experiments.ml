(*
  this is an old makefile example left for p4 reference, availability unknown
  make experiments OCAMLBUILD_EXTRAFLAGS='-ppflag "camlp4of -DADAPTON_LOG "'
*)

let default_outfile = "out/experiments.csv"
open Adapton_core
module Viz = Viz
module Statistics = AdaptonStatistics
module Types = AdaptonTypes

(** ---------------------------------------------------------------------- *)
(** Flags and Params that are common to all experiments: *)

module type FlagsType = sig
  val print_inout : bool
  val print_changes : bool
  val check_output : bool  (* checks for correctness after each change. is slower, as a result. *)
end

module Debug_flags : FlagsType = struct
  let print_inout   = true
  let print_changes = true
  let check_output  = true
end

module Test_flags : FlagsType = struct
  let print_inout   = false
  let print_changes = false
  let check_output  = true
end

module Perf_flags : FlagsType = struct
  let print_inout   = false
  let print_changes = false
  let check_output  = false
end

module type ParamsType = sig
  val sample_num : int   (* Determine seed. *)
  val n : int            (* Length of input lists *)
  val num_changes : int  (* Number of changes to perform on each input list *)
  val demand : float     (* Percent of output to demand to force after each change *)
  val num_lists : int    (* Number of distinct input lists. *)
  val fullinit : bool    (* Demand the full list on init *)
  val granularity : int  (* Number of non-articulated elements articulation is 2^g, on average *)
  val interactions : string list (* "r", "rr", "di", "id", "ss" *)
  val experiment : string
  val outfile : string
  module Flags : FlagsType
end

(** ---------------------------------------------------------------------- *)
(** Define the CSV output format: *)

(* the cvs file headers, keep this coordinated with the next function *)
let stats_labels_print (handle:out_channel) =
  Printf.fprintf handle
    "%s,%s,%s,%s,%s, %s,%s, %s,%s,\t %s, %s, %s, %s,\t %s,%s, %s,%s, %s,%s,  %s,%s, %s,\t %s\n%!"

    "Unix Time"
    "Seed"
    "Version"
    "Test"
    "Size"
    "Mod Pos" "Mod Pos %"
    "Demand" "Demand %"
    (* tab *)
    "Time"
    "Unit Cost"
    "Heap"
    "Stack"
    (* tab *)
    "dirty" "dirty %"
    "clean" "clean %"
    "evaluate" "evaluate %"

    "create" "create %"
    "tables"
    (* tab *)
    "granularity"
    ;
  flush handle

(* the csv file data, keep this coordinated with the previous function *)
let stats_print (handle:out_channel)
    (sample_num:int)
    (sysname:string) (interaction_desc:string)
    (inputsize:int) (initial_dcg_size:int)
    (change_pos:int) (demand_count:int)
    (demand_percent:float) (granularity:int)
    (stats:Statistics.t) : unit
    =
  let module Stats = Statistics in
  let percent x = ((float_of_int x) /. (float_of_int initial_dcg_size)) *. 100.0 in
  Printf.fprintf handle
    (* "|%d|%d|%s|%s|%d| %d|%.1f| %d|%.1f|\t %f| %d| %d| %d|\t %d|%.1f| %d|%.1f| %d|%.1f|  %d|%.1f| %d|\n%!" *)
    "%d,%d,%s,%s,%d, %d,%.1f, %d,%.1f,\t %f, %d, %d, %d,\t %d,%.1f, %d,%.1f, %d,%.1f,  %d,%.1f, %d,\t %d\n%!"

    (int_of_float (Unix.time())) (* sanity check, resolution in seconds (since 19700101) *)
    sample_num
    sysname
    interaction_desc
    inputsize
    change_pos ((* change_pos as a percent: *) (float_of_int change_pos) /. (float_of_int inputsize))
    demand_count demand_percent
    (* tab *)
    stats.Stats.time
    stats.Stats.unit_cost
    stats.Stats.heap
    stats.Stats.stack
    (* tab *)
    (* -- Should be a small percent of total graph: *)
    stats.Stats.dirty     (percent stats.Stats.dirty)
    stats.Stats.clean     (percent stats.Stats.clean)
    stats.Stats.evaluate  (percent stats.Stats.evaluate)
    (* -- Should be zero, or nearly zero: *)
    stats.Stats.create    (percent stats.Stats.create)
    stats.Stats.tables
    (* tab *)
    (* additional data here to maintain backwards compatability *)
    granularity
    ;
  flush handle

(* ---------------------------------------------------------------------- *)

module type ListRepType = sig
  type t
  module St : SpreadTree.SpreadTreeType
  module Data : Primitives.ResultType (* XXX *) with type t = int
  module Memotables : Primitives.MemotablesType
  type elm

  val of_list : Data.t list -> int -> t
  val next : t -> t option
  val last : t -> t

  val elm_of_int : int -> elm
  val elm_update : elm -> Data.t -> elm

  val delete_elm : t -> elm
  val insert_elm : t -> elm -> unit

  val set_art : t -> St.List.Data.t -> unit
  val force_art : t -> St.List.Data.t

  val string_of_list : t -> string

  val data_of_elm : elm -> Data.t
  val string_of_elm : elm -> string

  val take : t -> int option -> Data.t list
end

module type ListAppType = sig
  module ListRep : ListRepType
  val name    : string
  val compute : ListRep.t -> ListRep.t
  val trusted : ListRep.Data.t list -> ListRep.Data.t list
  val flush   : unit -> unit
end

module Make_experiment ( ListApp : ListAppType ) = struct
  module Memotables = ListApp.ListRep.Memotables

  (* ---------------------------------------------------------------------- *)
  IFDEF ADAPTON_LOG THEN
  let cntr = ref 0
  let output_graphstate ?label:(label="") out =
    Printf.fprintf out "[state](%d) %s\n%!" (!cntr) label ; incr cntr ;
    let print_edge src tgt edge_label =
      Printf.fprintf out "[edge %d %d %s]\n%!"
        (*memo_table_id*) src.Viz.id tgt.Viz.id edge_label
    in
    let pp_flag f = match f with
      | `Clean -> "clean"
      | `Dirty -> "dirty"
      | `Dirty_to_clean -> "dirtytoclean"
      | `Obsolete -> "obsolete"
    in
    let passes = [`Nodes_and_create_edges; `Force_edges] in
    List.iter begin fun pass ->
      match pass with
      | `Nodes_and_create_edges ->
        Printf.fprintf out "[]Graph layer: Nodes and create edges\n%!" ;
        Memotables.fold () begin
          fun () (module Mt) ->
            Mt.fold () begin fun () src ->
            (* Is src evicted or not? *)
              Printf.fprintf out "[node %d %s--%s]\n%!" src.Viz.id
                (match src.Viz.refc () with
                | None -> "refc-none"
                | Some n when n < 0 -> "refc-neg"
                | Some 0 -> "refc-zero"
                | Some 1 -> "refc-one"
                | Some 2 -> "refc-two"
                | Some _ -> "refc-many")
                (match src.Viz.finalizers () with
                | n when n < 0 -> "fin-neg"
                | 0 -> "fin-zero"
                | 1 -> "fin-one"
                | 2 -> "fin-two"
                | _ -> "fin-many")
              ;
              List.iter begin fun tgt ->
                print_edge src tgt "create" end
                (List.map fst src.Viz.mut_tgts)
            end
        end

      | `Force_edges ->
        Printf.fprintf out "[]Graph layer: Force edges\n%!" ;
        Memotables.fold () begin
          fun () (module Mt) ->
            Mt.fold () begin fun () src ->
              List.iter begin fun (tgt,flag) ->
                print_edge src tgt ("force-"^(pp_flag(flag())))
              end
                src.Viz.force_tgts
            end
        end
    end
    passes
  ELSE
  let output_graphstate ?label:(label="") out = ()
  ENDIF

  (* ---------------------------------------------------------------------- *)

  let name = ListApp.name
  let demand_list = ListApp.ListRep.take

  module T = Types
  module Stats = Statistics
  module Log = Log

  let gen_list n acc =
    let rec gen_list size acc =
      if size == 0 then
        acc
      else
        (* choose r based on n, since diverse input numbers are nice;
           want expected number of outcomes of comparisons to be equal
           (either less-than-or-equal or greater-than). *)
        let r = Random.int (128*n) in
        gen_list (size - 1) (r::acc)
    in gen_list n acc

  let string_of_list xs =
    List.fold_right (fun x y -> string_of_int x ^ "::" ^ y) xs "[]"

  let run (params:(module ParamsType)) = (
    let graphout = open_out ("graphmovie."^ ListApp.name ^".gmv") in
    let module Params = (val params) in

    (* note: the 0o666 below gives rw-r--r-- permissions, as does 0o664, I don't understand it, but it's sufficent *)
    let handle = open_out_gen [Open_creat;Open_append] 0o666 Params.outfile in

    (* let random_input_int ()   = Random.int (Params.n * 32) in *)
    let random_new_elm_int () = (Params.n * 128) + (Random.int (Params.n * 32)) in

    let benchmark_demand name n roundi handle graphout l computation final_art initial_dcg_size =
      let input_art = l in
      let output_art = computation in

      let assert_correct_output changedesc changeposi changepos =
        let xs = ListApp.trusted (demand_list l None) in
        let ys = demand_list computation None in
        if ( xs = ys ) then
          Printf.fprintf stdout "Checked.%!\n"
        else begin
          Printf.fprintf handle "%% %s: %d: Error: output after last %s is not correct.\n%!" name roundi changedesc;
          Printf.fprintf stdout "%s: %d: Error: not equal:\n   input:\t%s\n%s @ %d:\t%s\nexpected:\t%s\n    got:\t%s\n%!"
            name roundi
            (string_of_list (demand_list l None))
            changedesc changeposi
            (string_of_list (demand_list changepos None))
            (string_of_list xs)
            (string_of_list ys)
          ;
          assert false
        end
      in

      (* Pre-compute the places in the input list that we will change over time. *)
      let change_locations =
        let locations = Array.make Params.num_changes ( 0, l) in
        let n = float_of_int Params.n in
        let chs = float_of_int Params.num_changes in
        let gran = 2. ** (float_of_int Params.granularity) in
        let divs = n /. gran in
        let stride = divs /. chs in
        let rec build_change_locs art loc change art_count =
          if change >= Params.num_changes then locations else
          if loc >= 1. then
            match ListApp.ListRep.next art with
            | None -> locations
            | Some(a) ->
              build_change_locs a (loc-.1.0) change (art_count+.1.)
          else (
            let aprox_change_position = int_of_float (art_count *. gran) in
            locations.(change) <- (aprox_change_position, art);
            build_change_locs art (loc+.stride) (change+1) art_count
          )
        in
        build_change_locs l 0.0 0 0.0
      in

      let print_inout msg =
        if Params.Flags.print_inout then (
          Printf.printf "%s: input:\t%s\n" msg (string_of_list (demand_list input_art None));
          Printf.printf "%s: output:\t%s\n" msg (string_of_list (demand_list output_art None));
        ) else ()
      in

      let do_delete_insert_interaction pos l' demand_count =
        let rem_msg elm =
          Printf.sprintf "di-delete %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm elm) pos demand_count in
        (* Delete element. *)
        let (elm,_), deleteStats = Stats.measure begin fun () ->
          let elm = ListApp.ListRep.delete_elm l' in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (rem_msg elm) ;
	        elm, (demand_list computation (Some demand_count))
        end
        in
        print_inout (rem_msg elm) ;
        if Params.Flags.check_output then assert_correct_output (rem_msg elm) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(rem_msg elm) graphout ;
        end ;
        let ins_msg elm =
          Printf.sprintf "di-insert %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm elm) pos demand_count in
        (* Reinsert element. *)
        let _, insertStats = Stats.measure begin fun () ->
          (* Demand list. *)
          ListApp.ListRep.insert_elm l' elm ;
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (ins_msg elm) ;
          demand_list computation (Some demand_count)
        end
        in
        print_inout (ins_msg elm) ;
        if Params.Flags.check_output then assert_correct_output (ins_msg elm) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(ins_msg elm) graphout ;
        end ;
        [("di-delete",deleteStats); ("di-insert",insertStats)]
      in

      let do_insert_delete_interaction pos l' demand_count =
        let new_elm = ListApp.ListRep.elm_of_int (random_new_elm_int ()) in
        let ins_msg elm =
          Printf.sprintf "id-insert %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm new_elm) pos demand_count in
        (* Insert a (pathological!) element. *)
        let _, insertStats = Stats.measure begin fun () ->
	        (* Demand list. *)
          ListApp.ListRep.insert_elm l' new_elm ;
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (ins_msg new_elm) ;
          demand_list computation (Some demand_count)
        end
        in
        print_inout (ins_msg new_elm) ;
        if Params.Flags.check_output then assert_correct_output (ins_msg new_elm) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(ins_msg new_elm) graphout ;
        end ;
        let rem_msg elm =
          Printf.sprintf "id-delete %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm elm) pos demand_count in
        (* Delete element. *)
        let (elm,_), deleteStats = Stats.measure begin fun () ->
          let elm = ListApp.ListRep.delete_elm l' in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (rem_msg elm) ;
          elm, (demand_list computation (Some demand_count))
        end
        in
        print_inout (rem_msg elm) ;
        if Params.Flags.check_output then assert_correct_output (rem_msg elm) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(rem_msg elm) graphout ;
        end ;
        [("id-insert",insertStats); ("id-delete",deleteStats)]
      in

      let do_replace_interaction pos l' demand_count =
        let new_int = (random_new_elm_int ()) in
        let new_elm = ListApp.ListRep.elm_of_int new_int in
        let replace_msg elm =
          Printf.sprintf "r-replace %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm new_elm) pos demand_count in
        (* Insert a (pathological!) element. *)
        let _, replaceStats = Stats.measure begin fun () ->
          let elm = ListApp.ListRep.delete_elm l' in
          let elm = ListApp.ListRep.elm_update elm new_int in
          let _ = ListApp.ListRep.insert_elm l' elm in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (replace_msg new_elm) ;
          demand_list computation (Some demand_count)
        end
        in
        print_inout (replace_msg new_elm) ;
        if Params.Flags.check_output then assert_correct_output (replace_msg new_elm) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(replace_msg new_elm) graphout ;
        end ;
        [("r-replace",replaceStats)]
      in

      let do_replace_replace_interaction pos l' demand_count =
        let new_int = (random_new_elm_int ()) in
        let new_elm = ListApp.ListRep.elm_of_int new_int in
        let replace_msg num elm =
          Printf.sprintf "rr-replace%d %s @ %d; demand %d"
            num (ListApp.ListRep.string_of_elm new_elm) pos demand_count in
        (* Insert a (pathological!) element. *)
        let old, replace1Stats = Stats.measure begin fun () ->
          let elm = ListApp.ListRep.delete_elm l' in
          let old = ListApp.ListRep.data_of_elm elm in
          let elm = ListApp.ListRep.elm_update elm new_int in
          let _ = ListApp.ListRep.insert_elm l' elm in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (replace_msg 1 new_elm) ;
          let _ = demand_list computation (Some demand_count) in
          old
        end in
        print_inout (replace_msg 1 new_elm) ;
        if Params.Flags.check_output then assert_correct_output (replace_msg 1 new_elm) pos l' ;

        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(replace_msg 1 new_elm) graphout ;
        end ;

        let _, replace2Stats = Stats.measure begin fun () ->
          let elm = ListApp.ListRep.delete_elm l' in
          let elm = ListApp.ListRep.elm_update elm old in
          let _ = ListApp.ListRep.insert_elm l' elm in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (replace_msg 2 new_elm) ;
          demand_list computation (Some demand_count)
        end in
        print_inout (replace_msg 2 new_elm) ;
        if Params.Flags.check_output then assert_correct_output (replace_msg 2 new_elm) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(replace_msg 2 new_elm) graphout ;
        end ;

        [("rr-replace1",replace1Stats); ("rr-replace2",replace2Stats)]
      in

      let do_swap_swap_interaction pos l' demand_count =
        let msg elm1 elm2 =
          Printf.sprintf "ss-swap1 %s, %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm elm1) (ListApp.ListRep.string_of_elm elm2) pos demand_count in
        let (elm1,elm2), swap1Stats = Stats.measure begin fun () ->
          let elm1 = ListApp.ListRep.delete_elm l' in
          let elm2 = ListApp.ListRep.delete_elm l' in
          let elm1_data = ListApp.ListRep.data_of_elm elm1 in
          let elm2_data = ListApp.ListRep.data_of_elm elm2 in
          let elm1 = ListApp.ListRep.elm_update elm1 elm2_data in
          let elm2 = ListApp.ListRep.elm_update elm2 elm1_data in
          let _ = ListApp.ListRep.insert_elm l' elm2 in
          let _ = ListApp.ListRep.insert_elm l' elm1 in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (msg elm1 elm2) ;
          ignore ( demand_list computation (Some demand_count) );
          elm1, elm2
        end in
        print_inout (msg elm1 elm2) ;
        if Params.Flags.check_output then assert_correct_output (msg elm1 elm2) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(msg elm1 elm2) graphout ;
        end ;
        let msg elm1 elm2 =
          Printf.sprintf "ss-swap2 %s, %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elm elm1) (ListApp.ListRep.string_of_elm elm2) pos demand_count in
        let (elm1,elm2), swap2Stats = Stats.measure begin fun () ->
          let elm1 = ListApp.ListRep.delete_elm l' in
          let elm2 = ListApp.ListRep.delete_elm l' in
          let elm1_data = ListApp.ListRep.data_of_elm elm1 in
          let elm2_data = ListApp.ListRep.data_of_elm elm2 in
          let elm1 = ListApp.ListRep.elm_update elm1 elm2_data in
          let elm2 = ListApp.ListRep.elm_update elm2 elm1_data in
          let _ = ListApp.ListRep.insert_elm l' elm2 in
          let _ = ListApp.ListRep.insert_elm l' elm1 in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (msg elm1 elm2) ;
          ignore ( demand_list computation (Some demand_count) ) ;
          elm1, elm2
        end
        in
        print_inout (msg elm1 elm2) ;
        if Params.Flags.check_output then assert_correct_output (msg elm1 elm2) pos l' ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(msg elm1 elm2) graphout ;
        end ;
        [("ss-swap1",swap1Stats); ("ss-swap2",swap2Stats)]
      in

      let do_bigswap_interaction pos mid_art demand_count =
        let msg () =
          Printf.sprintf "bs-bswap1 %d; demand %d" pos demand_count
        in
        let _, bswap1Stats = Stats.measure begin fun () ->
          if not (input_art == mid_art) then (
            let temp = ListApp.ListRep.force_art input_art in
            ListApp.ListRep.set_art input_art (ListApp.ListRep.force_art mid_art);
            ListApp.ListRep.set_art mid_art `Nil;
            ListApp.ListRep.set_art final_art temp;
          );
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (msg ()) ;
          ignore ( demand_list computation (Some demand_count) );
        end in
        print_inout (msg ());
        if Params.Flags.check_output then assert_correct_output (msg ()) pos mid_art ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(msg ()) graphout ;
        end ;
        let msg () =
          Printf.sprintf "bs-bswap2 %d; demand %d" pos demand_count
        in
        let _, bswap2Stats = Stats.measure begin fun () ->
          if not (input_art == mid_art) then (
            let temp = ListApp.ListRep.force_art input_art in
            ListApp.ListRep.set_art input_art (ListApp.ListRep.force_art final_art);
            ListApp.ListRep.set_art mid_art temp;
            ListApp.ListRep.set_art final_art `Nil;
          );
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (msg ()) ;
          ignore ( demand_list computation (Some demand_count) ) ;
        end
        in
        print_inout (msg ()) ;
        if Params.Flags.check_output then assert_correct_output (msg ()) pos mid_art ;
        if Params.Flags.print_changes then begin
          Memotables.print_stats stdout ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(msg ()) graphout ;
        end ;
        [("bs-bswap1",bswap1Stats); ("bs-bswap2",bswap2Stats)]
      in

      let rec benchmark_demand (demand:float) = if demand <= 0.0 then () else
          let demand_count = int_of_float (( float Params.n) *. (demand /. 100.0)) in
          let demand_count = if demand_count = 0 then 1 else demand_count in
          for i = 0 to Params.num_changes - 1 do
	          let change_pos_idx, change_pos_inp = change_locations.(i) in
            let stats_list1 = if List.mem "r"  Params.interactions then do_replace_interaction         change_pos_idx change_pos_inp demand_count else [] in
            let stats_list2 = if List.mem "rr" Params.interactions then do_replace_replace_interaction change_pos_idx change_pos_inp demand_count else [] in
            let stats_list3 = if List.mem "di" Params.interactions then do_delete_insert_interaction   change_pos_idx change_pos_inp demand_count else [] in
            let stats_list4 = if List.mem "id" Params.interactions then do_insert_delete_interaction   change_pos_idx change_pos_inp demand_count else [] in
            let stats_list5 = if List.mem "ss" Params.interactions then do_swap_swap_interaction       change_pos_idx change_pos_inp demand_count else [] in
            let stats_list6 = if List.mem "bs" Params.interactions then do_bigswap_interaction         change_pos_idx change_pos_inp demand_count else [] in
            List.iter (fun (interaction_desc,stats) ->
              stats_print handle Params.sample_num name interaction_desc
                Params.n initial_dcg_size
                change_pos_idx demand_count demand Params.granularity stats)
              ( stats_list1 @ stats_list2 @ stats_list3 @ stats_list4 @ stats_list5 @ stats_list6) ;
          done;
          (* HACK -- print the final 'live works' right away, with the value in the heap position *)
          if true then (
            Printf.fprintf handle
              "%d,%d,%s,%s,%d, %d,%.1f, %d,%.1f,\t %f, %d, %d, %d,\t %d,%.1f, %d,%.1f, %d,%.1f, %d,%.1f, %d,\t %d\n%!"
              (int_of_float (Unix.time())) (* sanity check, resolution in seconds (since 19700101) *)
              Params.sample_num name
              "final"
              Params.n 0 0.0
              demand_count demand
            (* tab *)
              0.0 0
              (Gc.stat ()).Gc.live_words
              0
              (* tab *)
              0 0.0
              0 0.0
              0 0.0
              0 0.0
              0 Params.granularity;
          );
          ListApp.flush ();
          Pervasives.flush handle
      in
      benchmark_demand Params.demand
    in

    (* the code originally ran multiple times, printing "%% major GC." thousands of times per GC *)
    (* TODO: make one create_alarm call before running other code *)
    if false then (
      ignore (Gc.create_alarm begin fun _ ->
        if false then (
          Printf.fprintf handle "%% major GC: %d live words." (Gc.stat ()).Gc.live_words
        (* Note: Gc.stat will traverse the *whole heap*; thus, it affects performance and timings. *)
        ) else (
          Printf.fprintf handle "%% major GC."
        ) ;
        (*Gc.print_stat handle ; *)
        Printf.fprintf handle "\n%!"
      end)
    );

    (* Initially, tables should all be empty: *)
    Memotables.print_stats stdout ;
    output_graphstate ~label:"Empty graph" graphout ;

    (* make file header *)
    stats_labels_print handle;

    (* Do the run(s), for the number of lists requested: *)
    for i = 0 to Params.num_lists - 1 do
      Random.init (Params.sample_num + i); (* Fix seed. *)
      let raw_input = gen_list Params.n [] in
      let input = ListApp.ListRep.of_list raw_input Params.granularity in
      let final_art = ListApp.ListRep.last input in
      if Params.Flags.print_inout then begin
        Printf.printf "%d: Initial input:\t%s\n\n" i (ListApp.ListRep.string_of_list input);
      end;
      let demand_count =
        if Params.fullinit then
          (* demand all *)
          Params.n
        else
        (* only request the same amount as the incremental test will, as a lazy baseline *)
        let calc_demand = int_of_float (( float Params.n) *. (Params.demand /. 100.0)) in
        if calc_demand = 0 then 1 else calc_demand
      in
      let demand_percent = (float demand_count) /. (float Params.n) *. 100.0 in
      (* measure both the creation and initial computation of result *)
      let (_,output),s = Stats.measure (fun () ->
        let output = ListApp.compute input in
          (demand_list output (Some demand_count)), output)
      in
      let line_prefix = Printf.sprintf "%d:%s" Params.sample_num name in
      stats_print handle Params.sample_num name "initial" Params.n s.Stats.create 0 demand_count demand_percent Params.granularity s ;
      if Params.Flags.print_inout then begin
        Printf.printf "%d: Initial output:\t%s\n" i (ListApp.ListRep.string_of_list output);
      end;
      Printf.printf "\n----------------------------------------------------------------------------------\n%!" ;
      Printf.printf "%s: Initial run:\ttime:%.4fs (unit cost %d)\n" line_prefix s.Stats.time s.Stats.unit_cost ;
      Printf.printf "%s: Initial run:\t{dirty:%d; clean:%d; eval:%d; create:%d; tables:%d}\n%!"
        line_prefix s.Stats.dirty s.Stats.clean s.Stats.evaluate s.Stats.create s.Stats.tables ;
      Memotables.print_stats stdout ;
      Viz.flush_ticks_out graphout ;
      let (in_str,out_str) =
        if Params.Flags.print_inout then
          (string_of_list (demand_list input None)),
          (string_of_list (demand_list output None))
        else
          ("<omitted>","<omitted>")
      in
      output_graphstate ~label:(Printf.sprintf "Initial graph: mergesort %s => %s" in_str out_str) graphout ;
      let total_size = s.Stats.create in
      if Params.num_changes = 0 then (
        Printf.printf "--------------------\n%s: No changes requested; Skipping interaction.\n%!" line_prefix ;
      ) else (
        Printf.printf "--------------------\n%s: Interacting..\n%!" line_prefix ;
        let interact_begin_time = Unix.gettimeofday () in
        if true then
          benchmark_demand name Params.n i handle graphout input output final_art total_size
        else
          (try
             benchmark_demand name Params.n i handle graphout input output final_art total_size
           with
           | exn -> (
             Viz.flush_ticks_out graphout ;
             output_graphstate ~label:(Printf.sprintf "Exception: %s" (Printexc.to_string exn)) graphout ;
             raise exn
           )) ;
        let interact_end_time = Unix.gettimeofday () in
        Printf.printf "%s: Iteraction time: %f (sec)\n" line_prefix (interact_end_time -. interact_begin_time) ;
      );
      Memotables.print_stats stdout ;
      ListApp.flush () ;
      Viz.flush_ticks_out graphout ;
      output_graphstate ~label:"Final graph, flushed." graphout;
      IFDEF ADAPTON_LOG THEN (
        for i = 1 to 10 do
          (* Force garbage collection. Finalisers run. Flush DCG garbage. Flush trace.*)
          Gc.full_major ();
          ListApp.flush () ;
          Viz.flush_ticks_out graphout ;
          output_graphstate ~label:(Printf.sprintf "Final graph, full GC #%d." i) graphout;
        done
      ) ENDIF ;
    done;
    close_out handle;
  ) (* run *)
end (* Make_experiment *)


(* ------------------------------------------------------------------------------- *)

module Key = Key
module Int = Types.Int

module SpreadTreeRep
  (ArtLib : GrifolaType.ArtLibType)
  (* : ListRepType *) =
struct
  module ArtLib = ArtLib
  module St = SpreadTree.MakeSpreadTree(ArtLib)(Key)(Int)
  module Seq = SpreadTree.MakeSeq(St)
  module Data = Int
  module Memotables = ArtLib.Memotables
  module KvMap = SpreadTree.MakeKvMap(ArtLib)(Key)(struct include Int let compare = compare end)(St)

  type t   = St.List.Art.t
  type elm = Data.t

  let of_list x gran =
    Seq.mut_elms_of_list
      (* ~c=false *) (* true/default: Cons-Name-Art, false: Name-Cons-Art *)
      (Key.nondet())
      (List.map (fun x -> (x, Key.nondet())) x)
      fst snd gran

  let next x = Seq.next_art (St.List.Art.force x)

  let take x count = Seq.take (St.List.Art.force x) count

  let delete_elm list =
    let h,_ = Seq.delete_elm list in
    h

  let last list =
    Seq.get_or_create_final_art list

  (* needed to break the higher level abstraction to swap whole chunks of a list *)
  let set_art art data = Seq.LArt.set art data
  let force_art art = Seq.LArt.force art

  let insert_elm list h =
    Seq.insert_elm list h None

  let string_of_list art = Seq.simple_full_string (`Art art)

  let elm_of_int h = h
  let elm_update x y = y
  let data_of_elm x = x
  let string_of_elm x = Printf.sprintf "%d" x
end

module RepOfSpreadTree
  (St : SpreadTree.SpreadTreeType)
  (* : ListRepType *) =
struct
  module St = St
  module ArtLib = St.ArtLib
  module Name = St.Name
  module Seq = SpreadTree.MakeSeq(St)

  module Memotables = ArtLib.Memotables
  module KvMap = SpreadTree.MakeKvMap(ArtLib)(Name)(struct include Int let compare = compare end)(St)

  type t      = KvMap.KvSt.List.Art.t
  module Data = KvMap.KvSt.Data

  type elm = Data.t

  let of_list x =
    KvMap.KvSeq.mut_elms_of_list (Name.nondet())
      (List.map (fun x -> (x, Name.nondet())) x)
      fst snd

  let next x = KvMap.KvSeq.next_art (KvMap.KvSt.List.Art.force x)
  let last x = failwith "unimplemented"

  let take x count = KvMap.KvSeq.take (KvMap.KvSt.List.Art.force x) count

  let delete_elm list =
    let h,_ = KvMap.KvSeq.delete_elm list in
    h

  let insert_elm list h =
    KvMap.KvSeq.insert_elm list h None

  let force_art art = failwith "unimplemented"
  let set_art art data = failwith "unimplemented"

  let string_of_list x = "not implemented"

  let elm_of_int h = h
  let elm_update x y = y
  let data_of_elm x = x
  let string_of_elm x = Printf.sprintf "%d" x
end

(* ------------------------------------------------------------------------------- *)

(* module AKListRepGrifola = struct
  module Grifola = Grifola.Default
  module AKList  = AdaptonUtil.AKList.Make( Grifola.ATypeImpl )
  module IL = AKList.Make( Int )
  module Memotables = Grifola.Memotables
  type t = IL.t
  module Data = Int
  type elm = Data.t * Key.t * IL.t

  let next l = match IL.force l with
    | `Nil -> None
    | `Cons(_,_,tl) -> Some tl

  let of_list ints = IL.of_list ints

  let take l optional_max0 =
    let rec demand_list l optional_max =
      match (IL.force l), optional_max with
        | `Nil, None   -> []
        | _   , Some 0 -> []
        | `Nil, Some n ->
            let n0 = match optional_max0 with Some n -> n | None -> failwith "impossible" in
	    Printf.fprintf stdout "Warning: reached end of list before demanding all elements: demanded %d of %d\n%!" (n0-n) n0 ;
            []

        | `Cons(x, _, t), Some n ->
            x :: (demand_list t (Some (n-1)))
        | `Cons(x, _, t), None ->
            x :: (demand_list t None)
    in
    demand_list l optional_max0

  let delete_elm list =
    let (h,k,tl) = IL.remove' 0 list in (h,k,tl)

  let insert_elm list (h,k,tl) =
    IL.insert' 0 h k list tl

  let data_of_elm (h,_,_) = h
  let string_of_elm (h,k,tl) = Printf.sprintf "%d %s" h (Key.string k)

  let elm_of_int h = (h,Key.nondet(),IL.const `Nil)

  let elm_update (_,key,tl) x = (x,key,tl)
end
 *)
(* ------------------------------------------------------------------------------- *)

(* module AKListRep ( A : AdaptonUtil.Signatures.AType ) = struct
  module Data = Int
  module AKList = AdaptonUtil.AKList.Make( A )
  module IL = AKList.Make( Data )
  type t = IL.t
  type elm = Data.t * Key.t * IL.t
  module Memotables = A.Memotables

  let of_list ints = IL.of_list ints

  let next l = match IL.force l with
    | `Nil -> None
    | `Cons(_,_,tl) -> Some tl

  let take l optional_max0 =
    let rec demand_list l optional_max =
      match (IL.force l), optional_max with
        | `Nil, None   -> []
        | _   , Some 0 -> []
        | `Nil, Some n ->
            let n0 = match optional_max0 with Some n -> n | None -> failwith "impossible" in
	    Printf.fprintf stdout "Warning: reached end of list before demanding all elements: demanded %d of %d\n%!" (n0-n) n0 ;
            []

        | `Cons(x, _, t), Some n ->
            x :: (demand_list t (Some (n-1)))
        | `Cons(x, _, t), None ->
            x :: (demand_list t None)
    in
    demand_list l optional_max0

  let delete_elm list =
    let (h,k,tl) = IL.remove' 0 list in (h,k,tl)

  let insert_elm list (h,k,tl) =
    IL.insert' 0 h k list tl

  let data_of_elm (h,_,_) = h
  let string_of_elm (h,k,tl) = Printf.sprintf "%d %s" h (Key.string k)

  let elm_of_int h = (h,Key.nondet(),IL.const `Nil)

  let elm_update (_,key,tl) x = (x,key,tl)
end
 *)
(* ------------------------------------------------------------------------------- *)
module Linear = struct
  module List_filter
    (N : sig val name : string end)
    (AL : GrifolaType.ArtLibType) =
  struct
    let name = "List_filter_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let filter_even x = x mod 2 = 0
    let compute inp =
      let nm1, nm2 = Key.fork (Key.nondet()) in
      let filter = ListRep.Seq.list_filter nm1 filter_even in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        filter (ListRep.St.List.Art.force inp)
      )
    let trusted = List.filter filter_even
    let flush = AL.Eviction.flush
  end

  module List_map
    (N : sig val name : string end)
    (AL : GrifolaType.ArtLibType) =
  struct
    let name = "List_map_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let almost_triple x = x * 2 - 1
    let compute inp =
      let nm1, nm2 = Key.fork (Key.nondet()) in
      let map = ListRep.Seq.list_map nm1 almost_triple in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        map (ListRep.St.List.Art.force inp)
      )
    let trusted = List.map almost_triple
    let flush = AL.Eviction.flush
  end

  module List_map_paired
    (N : sig val name : string end)
    (AL : GrifolaType.ArtLibType) =
  struct
    let name = "List_map_paired_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let compute inp =
      let nm1, nm2 = Key.fork (Key.nondet()) in
      let map = ListRep.Seq.list_map_paired nm1 (+) in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        map (ListRep.St.List.Art.force inp)
      )
    let trusted inp =
      let choose = ref false in
      let every_other _ =
        choose:= not !choose;
        !choose
      in
      let l1,l2 = List.partition every_other inp in
      (* TODO: update this to deal with different length of lists (insert/delete) *)
      List.map2 (+) l1 l2
    let flush = AL.Eviction.flush
  end

end

module Foo = struct
  module T = Trie.Set.Make(
                 struct
                   include Types.Int
                   let compare = Pervasives.compare
                 end  )
end

module Reverse = struct

  module Rope_reverse
    (N : sig val name : string end)
    (AL : GrifolaType.ArtLibType) =
  struct
    let name = "Rope_reverse_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = (Key.nondet ()) in
      let rev = ListRep.Seq.rope_reverse in
      let to_rope = ListRep.Seq.rope_of_list in
      let to_list = ListRep.Seq.list_of_rope in
      let force = ListRep.St.List.Art.force in
      ListRep.St.List.Art.thunk nm ( fun () ->
        to_list (rev (to_rope (force inp))) `Nil
      )
    let trusted = List.rev
    let flush = AL.Eviction.flush
  end

  module List_reverse
    (N : sig val name : string end)
    (AL : GrifolaType.ArtLibType) =
  struct
    let name = "List_reverse_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = (Key.nondet ()) in
      let rev = ListRep.Seq.list_reverse_balanced in
      let force = ListRep.St.List.Art.force in
      ListRep.St.List.Art.thunk nm ( fun () ->
                                     let res = rev(force inp) `Nil in
                                     Printf.printf "... Computed: %s\n" (ListRep.St.List.Data.string res) ;
                                     res
                                   )
    let trusted = List.rev
    let flush = AL.Eviction.flush
  end
end

module Mergesorts = struct

(*
  module AKList_mergesort (N : sig val name : string end) ( A : AdaptonUtil.Signatures.AType ) = struct
    let name = "AKList_mergesort_" ^ N.name
    module ListRep = AKListRep ( A )
    let compute inp =
      ListRep.IL.memo_mergesort Pervasives.compare (Key.nondet ()) inp
    let min_of_ints x y = (
      incr AdaptonUtil.Statistics.Counts.unit_cost ;
      if x < y then x else y
    )
    let trusted = List.sort Pervasives.compare
    let flush = ListRep.IL.flush
  end
 *)

  module List_mergesort
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "List_mergesort_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = Key.fork (Key.nondet ()) in
      let mergesort = ListRep.Seq.list_mergesort (fst nm) int_compare in
      ListRep.St.List.Art.thunk (snd nm) ( fun () ->
        mergesort (ListRep.St.List.Art.force inp)
      )
    let trusted = List.sort int_compare
    let flush = AL.Eviction.flush
  end

  module Rope_mergesort
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "Rope_mergesort_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = Key.fork (Key.nondet ()) in
      let mergesort = ListRep.Seq.list_to_rope_mergesort
                        (fst nm) int_compare in
      ListRep.St.List.Art.thunk (snd nm) ( fun () ->
        mergesort (ListRep.St.List.Art.force inp)
      )
    let trusted = List.sort int_compare
    let flush = AL.Eviction.flush
  end

end

module Median = struct

  module Rope_median
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "Rope_median_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm1, nm2 = Key.fork (Key.nondet()) in
      let mergesort = ListRep.Seq.rope_mergesort nm1 int_compare in
      let rope_of_list = ListRep.Seq.rope_of_list in
      let rope_nth = ListRep.Seq.rope_nth in
      let rope_len = ListRep.Seq.rope_length in
      let force = ListRep.St.List.Art.force in
      ListRep.St.List.Art.thunk (nm2) ( fun() ->
        let inc_list = rope_of_list (force inp) in
        let mid = (rope_len inc_list)/2 in
        let sorted = rope_of_list @@ mergesort inc_list in
        let result = rope_nth sorted mid in
        match result with
        | None -> `Nil
        | Some x -> `Cons(x,`Nil)
      )
    let trusted inp =
      let len = List.length inp in
      let middle = len/2 in
      let sorted = List.sort int_compare inp in
      [List.nth sorted middle]
    let flush = AL.Eviction.flush
  end

  module Rope_center
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "Rope_center_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm1 = Key.nondet() in
      let rope_of_list = ListRep.Seq.rope_of_list in
      let rope_median = ListRep.Seq.rope_median in
      ListRep.St.List.Art.thunk (nm1) ( fun() ->
        let result =
          rope_median @@ rope_of_list @@
          (ListRep.St.List.Art.force inp)
        in
        match result with
        | None -> `Nil
        | Some x -> `Cons(x,`Nil)
      )
    let trusted inp =
      let len = List.length inp in
      let middle = len/2 in
      [List.nth inp middle]
    let flush = AL.Eviction.flush
  end
end

module Pointcloud = struct
  
  module List_Quickhull
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "Quickhull_" ^ N.name
    module ListRep = SpreadTreeRep(AL)
    module QH = Quickhull.StMake(ListRep.St)
    let compute inp =
      let nm1 = (Key.nondet()) in
      let quickhull = QH.list_quickhull in
      ListRep.St.List.Art.thunk nm1 ( fun () -> 
        quickhull (ListRep.St.List.Art.force inp)
      )
    let trusted = Quickhull.list_quickhull
    let flush = AL.Eviction.flush
  end

  module Rope_quickhull
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "Rope_quickhull_" ^ N.name
    module ListRep = SpreadTreeRep(AL)
    module QH = Quickhull.StMake(ListRep.St)
    let compute inp =
      let nm1 = (Key.nondet()) in
      let quickhull = QH.rope_quickhull in
      ListRep.St.List.Art.thunk nm1 ( fun () ->
        quickhull (ListRep.St.List.Art.force inp)
      )
    let trusted = Quickhull.list_quickhull
    let flush = AL.Eviction.flush
  end

end

module Iteration = struct
  module Rope_iter
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "Rope_iter_" ^ N.name
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let rope_of_list = ListRep.Seq.rope_of_list in
      let list_of_rope = ListRep.Seq.list_of_rope in
      ListRep.St.List.Art.thunk (Key.nondet ()) (
        fun () ->
          let rope = rope_of_list (ListRep.St.List.Art.force inp) in
          list_of_rope rope `Nil
      )
    let trusted = List.map (fun x -> x)
    let flush = AL.Eviction.flush
  end
end

module List_transf = struct
  let mapf x = int_of_float ((log (1. +. float_of_int x)) +. log 1.5)
  let filterf x = ((x mod 2) = 0)

end

module Reduction = struct

  module Rope_reduce_monoid
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType )
    ( Monoid : sig val name : string
                   val id_elm : int
                   val bin : int -> int -> int end ) =
  struct
    let name = "Rope_" ^ Monoid.name ^ "_" ^ N.name
    module ListRep = SpreadTreeRep ( AL )
    let min_of_ints x y = (
      incr Statistics.Counts.unit_cost ;
      Monoid.bin x y
    )
    let compute inp =
      let rope_reduce = ListRep.Seq.rope_reduce (Key.nondet()) min_of_ints in
      ListRep.St.List.Art.thunk (Key.nondet ()) ( fun () ->
          let rope = ListRep.Seq.rope_of_list (`Art inp) in
          match rope_reduce rope with
          | None -> `Nil
          | Some x -> `Cons(x,`Nil)
      )
    let trusted x = match x with
      | [] -> []
      | _ -> [ List.fold_left Monoid.bin Monoid.id_elm x ]
    let flush = AL.Eviction.flush
  end

  module Rope_min
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    include Rope_reduce_monoid(N)(AL)
      (struct
        let name = "min"
        let id_elm = max_int
        let bin x y = if x < y then x else y
       end)
  end

  module Rope_sum
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    include Rope_reduce_monoid(N)(AL)
      (struct
        let name = "sum"
        let id_elm = 0
        let bin x y = x + y
       end)
  end

  module AVL_of_rope (* works: gives advantage to nominal approach. *)
    ( N : sig val name : string end )
    ( AL : GrifolaType.ArtLibType ) =
  struct
    let name = "AVL_of_rope_" ^ N.name
    module St = SpreadTree.MakeSpreadTree(AL)(Key)(Int)
    module ListRep = RepOfSpreadTree ( St )
    let compute inp =
      let rope_of_list = ListRep.KvMap.KvSeq.rope_of_list in
      let name = (Key.nondet()) in
      ListRep.KvMap.KvSt.List.Art.thunk (Key.nondet ()) ( fun () ->
        let rope = rope_of_list (`Art inp) in
        let avl = ListRep.KvMap.avl_tree_of_rope name rope (`Leaf `Nil) in
        (ListRep.KvMap.KvSeq.list_of_tree avl `Nil)
      )
    let trusted x =
      let module S = Set.Make(struct type t = int let compare = Pervasives.compare end) in
      let set = List.fold_left (fun set elm -> S.add elm set) S.empty x in
      (S.elements set)
    let flush = AL.Eviction.flush
  end

end

(* ----------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------------------------- *)

module Engines = struct
  module Grifola_name = Grifola.Make(
    struct
      include Grifola.Default_params
    end )
  module Grifola_arg = Grifola.Make(
    struct
      include Grifola.Default_params
      let disable_names = true
    end )
  module Grifola_arggen = Grifola.Make(
    struct
      include Grifola.Default_params
      let disable_names = true
      let generative_ids = true
    end )
  module Grifola_nocheck = Grifola.Make(
    struct
      include Grifola.Default_params
      let check_receipt = false
    end )
  module EagerNonInc = Alternatives.EagerNonInc
  module LazyNonInc = Alternatives.LazyNonInc
  module LazyRecalc = Alternatives.LazyRecalc
  module Sac = Alternatives.Sac
end

module type ExperimentType = sig
  val name : string
  val run  : (module ParamsType) -> unit
end

module Experiments = struct
  open Engines

  module List_filter = struct
    module ListApp_name = Linear.List_filter(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Linear.List_filter(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Linear.List_filter(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_filter(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_filter(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_filter(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_filter(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_map = struct
    module ListApp_name = Linear.List_map(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Linear.List_map(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Linear.List_map(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_map(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_map(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_map(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_map(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_map_paired = struct
    module ListApp_name = Linear.List_map_paired(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Linear.List_map_paired(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Linear.List_map_paired(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_map_paired(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_map_paired(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_map_paired(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
  end

  module List_reverse = struct
    module ListApp_name = Reverse.List_reverse(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arggen = Reverse.List_reverse(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Reverse.List_reverse(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reverse.List_reverse(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_reverse = struct
    module ListApp_name = Reverse.Rope_reverse(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arggen = Reverse.Rope_reverse(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Reverse.Rope_reverse(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reverse.Rope_reverse(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_mergesort = struct
    module ListApp_name = Mergesorts.Rope_mergesort(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Mergesorts.Rope_mergesort(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Mergesorts.Rope_mergesort(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Mergesorts.Rope_mergesort(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Mergesorts.Rope_mergesort(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Mergesorts.Rope_mergesort(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Mergesorts.Rope_mergesort(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_mergesort = struct
    module ListApp_name = Mergesorts.List_mergesort(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Mergesorts.List_mergesort(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Mergesorts.List_mergesort(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Mergesorts.List_mergesort(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Mergesorts.List_mergesort(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_median = struct
    module ListApp_name = Median.Rope_median(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Median.Rope_median(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Median.Rope_median(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Median.Rope_median(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Median.Rope_median(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_sac = Median.Rope_median(struct let name = "sac" end)(Sac.ArtLib)
  end

  (* to test the median, this finds center without sorting first *)
  module Rope_center = struct
    module ListApp_name = Median.Rope_center(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Median.Rope_center(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Median.Rope_center(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Median.Rope_center(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Median.Rope_center(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_sac = Median.Rope_center(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Quickhull = struct
    module ListApp_name = Pointcloud.List_Quickhull(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arggen = Pointcloud.List_Quickhull(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Pointcloud.List_Quickhull(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Pointcloud.List_Quickhull(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_quickhull = struct
    module ListApp_name = Pointcloud.Rope_quickhull(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arggen = Pointcloud.Rope_quickhull(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Pointcloud.Rope_quickhull(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Pointcloud.Rope_quickhull(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_iter = struct
    module ListApp_name = Iteration.Rope_iter(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Iteration.Rope_iter(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Iteration.Rope_iter(struct let name = "arggen" end)(Grifola_name.ArtLib)
    module ListApp_sac = Iteration.Rope_iter(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_min = struct
    module ListApp_name = Reduction.Rope_min(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Reduction.Rope_min(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Reduction.Rope_min(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Reduction.Rope_min(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reduction.Rope_min(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_sum = struct
    module ListApp_name = Reduction.Rope_sum(struct let name = "name" end)(Grifola_name.ArtLib)
    module ListApp_arg = Reduction.Rope_sum(struct let name = "arg" end)(Grifola_arg.ArtLib)
    module ListApp_arggen = Reduction.Rope_sum(struct let name = "arggen" end)(Grifola_arggen.ArtLib)
    module ListApp_lazy_recalc = Reduction.Rope_sum(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reduction.Rope_sum(struct let name = "sac" end)(Sac.ArtLib)
  end

  module AVL_name = Reduction.AVL_of_rope(struct let name = "grifola_name" end)(Grifola_name.ArtLib)
  module AVL_arggen = Reduction.AVL_of_rope(struct let name = "grifola_arggen" end)(Grifola_arggen.ArtLib)
  module AVL_lazy_recalc = Reduction.AVL_of_rope(struct let name = "lazy_recalc" end)(LazyRecalc.ArtLib)

end

let raw_experiments =
[
  (* Rope min versions *)
  (module Experiments.Rope_min.ListApp_name           : ListAppType) ;
  (module Experiments.Rope_min.ListApp_arg            : ListAppType) ;
  (module Experiments.Rope_min.ListApp_arggen         : ListAppType) ;
  (module Experiments.Rope_min.ListApp_lazy_recalc     : ListAppType) ;
  (module Experiments.Rope_min.ListApp_sac             : ListAppType) ;

  (* Rope sum versions *)
  (module Experiments.Rope_sum.ListApp_name           : ListAppType) ;
  (module Experiments.Rope_sum.ListApp_arg            : ListAppType) ;
  (module Experiments.Rope_sum.ListApp_arggen         : ListAppType) ;
  (module Experiments.Rope_sum.ListApp_lazy_recalc     : ListAppType) ;
  (module Experiments.Rope_sum.ListApp_sac             : ListAppType) ;

  (* Rope iter versions *)
  (module Experiments.Rope_iter.ListApp_name           : ListAppType) ;
  (module Experiments.Rope_iter.ListApp_arg            : ListAppType) ;
  (module Experiments.Rope_iter.ListApp_arggen         : ListAppType) ;
  (module Experiments.Rope_iter.ListApp_sac             : ListAppType) ;

  (* List filter *)
  (module Experiments.List_filter.ListApp_name         : ListAppType) ;
  (module Experiments.List_filter.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_filter.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_filter.ListApp_sac             : ListAppType) ;

  (* List map *)
  (module Experiments.List_map.ListApp_name         : ListAppType) ;
  (module Experiments.List_map.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_map.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_map.ListApp_sac          : ListAppType) ;

  (* List map paired *)
    (* checking this will fail when list lengths are
    not even numbers, like after insert or delete *)
  (module Experiments.List_map_paired.ListApp_name         : ListAppType) ;
  (module Experiments.List_map_paired.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_map_paired.ListApp_lazy_recalc  : ListAppType) ;

  (* List_reverse *)
  (module Experiments.List_reverse.ListApp_name         : ListAppType) ;
  (module Experiments.List_reverse.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_reverse.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_reverse.ListApp_sac          : ListAppType) ;

  (* Rope_reverse *)
  (module Experiments.Rope_reverse.ListApp_name         : ListAppType) ;
  (module Experiments.Rope_reverse.ListApp_arggen       : ListAppType) ;
  (module Experiments.Rope_reverse.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.Rope_reverse.ListApp_sac          : ListAppType) ;

  (* Rope mergesort *)
  (module Experiments.Rope_mergesort.ListApp_name         : ListAppType) ;
  (module Experiments.Rope_mergesort.ListApp_arggen       : ListAppType) ;
  (module Experiments.Rope_mergesort.ListApp_eager_noninc : ListAppType) ;
  (module Experiments.Rope_mergesort.ListApp_lazy_noninc  : ListAppType) ;
  (module Experiments.Rope_mergesort.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.Rope_mergesort.ListApp_sac          : ListAppType) ;

  (* List mergesort *)
  (module Experiments.List_mergesort.ListApp_name         : ListAppType) ;
  (module Experiments.List_mergesort.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_mergesort.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_mergesort.ListApp_sac          : ListAppType) ;

  (* Rope Median *)
  (module Experiments.Rope_median.ListApp_name         : ListAppType) ;
  (module Experiments.Rope_median.ListApp_arggen       : ListAppType) ;
  (module Experiments.Rope_median.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.Rope_median.ListApp_eager_noninc : ListAppType) ;
  (module Experiments.Rope_median.ListApp_sac          : ListAppType) ;

  (* Rope Center (median without sorting) *)
  (module Experiments.Rope_center.ListApp_name         : ListAppType) ;
  (module Experiments.Rope_center.ListApp_arggen       : ListAppType) ;
  (module Experiments.Rope_center.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.Rope_center.ListApp_eager_noninc : ListAppType) ;
  (module Experiments.Rope_center.ListApp_sac          : ListAppType) ;

  (* Quickhull *)
  (module Experiments.Quickhull.ListApp_name         : ListAppType) ;
  (module Experiments.Quickhull.ListApp_arggen       : ListAppType) ;
  (module Experiments.Quickhull.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.Quickhull.ListApp_sac          : ListAppType) ;

  (* Rope_quickhull *)
  (module Experiments.Rope_quickhull.ListApp_name         : ListAppType) ;
  (module Experiments.Rope_quickhull.ListApp_arggen       : ListAppType) ;
  (module Experiments.Rope_quickhull.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.Rope_quickhull.ListApp_sac          : ListAppType) ;

  (* Benchmarks for overhead comparison: *)
  (module Experiments.AVL_name                         : ListAppType) ;
  (module Experiments.AVL_arggen                       : ListAppType) ;
  (module Experiments.AVL_lazy_recalc                  : ListAppType) ;

]

let all_experiments : (module ExperimentType) list =
  List.map
  (fun a ->
    (* apply Make_experiment functor to input modules *)
    let module App = (val a : ListAppType) in
    let module Exp = Make_experiment(App)in
    (module Exp : ExperimentType)
  )
  raw_experiments

module Default_perf_params : ParamsType = struct
  let sample_num = 0   (* Seed. *)
  let n = 5000         (* Length of input lists *)
  let num_changes = 10 (* Number of changes to perform on each input list *)
  let demand = 100.0    (* Percent of output to demand to force after each change *)
  let num_lists = 1    (* Number of distinct input lists. *)
  let fullinit = false
  let granularity = 0
  let interactions = [ "di"; "id"; "ss"; "rr"; "r"; "bs"]
  let experiment = ""
  let outfile = default_outfile
  module Flags = struct
    include Perf_flags
    (*let print_inout = true *)
  end
end

module Benchmark_params_10k : ParamsType = struct
  let sample_num = 0   (* Seed. *)
  let n = 10000        (* Length of input lists *)
  let num_changes = 10 (* Number of changes to perform on each input list *)
  let demand = 100.0    (* Percent of output to demand to force after each change *)
  let num_lists = 4    (* Number of distinct input lists. *)
  let fullinit = false
  let granularity = 0
  let interactions = [ "di"; "id"; "ss"]
  let experiment = ""
  let outfile = default_outfile
  module Flags = Perf_flags
end

module Movie_params : ParamsType = struct
  let sample_num = 0   (* Seed. *)
  let n = 16
  let num_changes = 8
  let demand = 50.0
  let num_lists = 1
  let fullinit = false
  let granularity = 0
  let interactions = [ "di"; "id"; "ss";"r"]
  let experiment = ""
  let outfile = default_outfile
  module Flags = Debug_flags
end

module Short_movie_params : ParamsType = struct
  let sample_num = 0   (* Seed. *)
  let n = 6           (* Length of input lists *)
  let num_changes = 3 (* Number of changes to perform on each input list *)
  let demand = 100.0  (* Percent of output to demand to force after each change *)
  let num_lists = 1   (* Number of distinct input lists. *)
  let fullinit = false
  let granularity = 0
  let interactions = [ "di"; "id"; "ss";"r"]
  let experiment = ""
  let outfile = default_outfile
  module Flags = Debug_flags
end

module Test_params : ParamsType = struct
  let sample_num = 0   (* Seed. *)
  let n = 1000          (* Length of input lists *)
  let num_changes = 500 (* Number of changes to perform on each input list *)
  let demand = 50.0     (* Percent of output to demand to force after each change *)
  let num_lists = 10    (* Number of distinct input lists. *)
  let fullinit = false
  let granularity = 0
  let interactions = [ "di"; "id"; "ss";"r"]
  let experiment = ""
  let outfile = default_outfile
  module Flags = Debug_flags
end

module Commandline_params : ParamsType = struct
  module Default = Default_perf_params

  let sample_num_   = ref Default.sample_num
  let n_            = ref Default.n
  let num_changes_  = ref Default.num_changes
  let demand_       = ref Default.demand
  let num_lists_    = ref Default.num_lists
  let fullinit_     = ref Default.fullinit
  let granularity_  = ref Default.granularity
  let flags_        = ref (module Default.Flags : FlagsType)
  let interactions_ = ref Default.interactions
  let experiment_   = ref Default.experiment
  let outfile_      = ref Default.outfile

  let args = [
    ("--sample-num",  Arg.Int   (fun n -> sample_num_ := n),  " sample number; determines random numbers as their seed.");
    ("--n",           Arg.Int   (fun n -> n_ := n),           " input size");
    ("--num-changes", Arg.Int   (fun n -> num_changes_ := n), " number of changes to input");
    ("--demand",      Arg.Float (fun n -> demand_ := n),      " percent of output to observe");
    ("--num-lists",   Arg.Int   (fun n -> num_lists_ := n),   " number of lists to try");
    ("--gran",        Arg.Int   (fun n -> granularity_ := n), " granularity of articulation points");
    ("--fullinit",    Arg.Set   fullinit_,                    " demand full list at initialization");
    ("--outfile",     Arg.String(fun s -> outfile_ := s),     " file to save output to");
    ("--experiment",  Arg.String(fun name_req ->
      ( let names =
          List.map (fun exp -> let module E = (val exp : ExperimentType) in E.name) (all_experiments)
        in
        let matches =
          (* TODO: Support globbing, for pattern-matching many experiments. *)
          List.filter (fun name -> name = name_req) names
        in
        match matches with
        | [] | (_ :: _ :: _) ->
          Printf.eprintf "Warning: Couldn't find a unique experiment with the requested name `%s'\n" name_req ;
          if List.length matches > 0 then (
            Printf.eprintf "The experiment name `%s' is not unique: Found %d matches total, but expected exactly one.\n" name_req (List.length matches);
          ) ;
          Printf.eprintf "  Try one of these:\n" ;
          List.iter (fun name -> Printf.eprintf "   %s\n" name) names

        | [req_name] -> experiment_ := req_name

      )), "choose the experiment by name");

    ("--0",   Arg.Unit  (fun () -> interactions_ := []),                     " clear interaction list");
    ("--di",  Arg.Unit  (fun () -> interactions_ := "di" :: !interactions_), " add interaction: delete, insert");
    ("--id",  Arg.Unit  (fun () -> interactions_ := "di" :: !interactions_), " add interaction: insert, delete");
    ("--r",   Arg.Unit  (fun () -> interactions_ := "r"  :: !interactions_), " add interaction: replace");
    ("--rr",  Arg.Unit  (fun () -> interactions_ := "rr" :: !interactions_), " add interaction: replace, replace");
    ("--ss",  Arg.Unit  (fun () -> interactions_ := "ss" :: !interactions_), " add interaction: swap, swap");
    ("--bs",  Arg.Unit  (fun () -> interactions_ := "bs" :: !interactions_), " add interaction: swap entire list");

    ("--perf-flags",  Arg.Unit  (fun () -> flags_ := (module Perf_flags)), " performance flags");
    ("--test-flags",  Arg.Unit  (fun () -> flags_ := (module Test_flags)), " testing flags");
    ("--debug-flags", Arg.Unit  (fun () -> flags_ := (module Debug_flags)), " debugging flags");
    ("-verbose",      Arg.Unit  (fun () -> ()), " ignored");
  ]

  let _ = Arg.parse args
    (fun arg -> invalid_arg ("Not recognized:"^arg))
    "usage: experiments [options]"

  let sample_num = !sample_num_
  let n = !n_
  let num_changes = !num_changes_
  let demand = !demand_
  let num_lists = !num_lists_
  let fullinit = !fullinit_
  let granularity = !granularity_
  let interactions = List.rev (!interactions_)
  let experiment = !experiment_
  let outfile = !outfile_
  module Flags = (val !flags_)
end

let _ =
  let requested_experiments =
    (List.filter (fun exp ->
      let module E = (val exp : ExperimentType) in
      E.name = Commandline_params.experiment)
       all_experiments)
  in
  List.iter (fun exp ->
    let module E = (val exp : ExperimentType) in
    Printf.eprintf "You requested experiment: %s\n%!" E.name
  ) requested_experiments
  ;
  List.iter (fun exp ->
    let module E = (val exp : ExperimentType) in
    E.run (module Commandline_params)
  ) requested_experiments
