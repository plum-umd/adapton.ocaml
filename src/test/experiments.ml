(*
  this is an old makefile example left for p4 reference, availability unknown
  make experiments OCAMLBUILD_EXTRAFLAGS='-ppflag "camlp4of -DADAPTON_LOG "'
*)

let default_outfile = "out/experiments.csv"

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
  module St : SpreadTree.S
  module Data : Data.S with type t = int
  module Memotables : MemoTable.REGISTRY
  type elt

  val of_list : Data.t list -> int -> t
  val next : t -> t option
  val last : t -> t

  val elt_of_int : int -> elt
  val elt_update : elt -> Data.t -> elt

  val delete_elt : t -> elt
  val insert_elt : t -> elt -> unit

  val set_art : t -> St.List.t -> unit
  val force_art : t -> St.List.t

  val string_of_list : t -> string

  val data_of_elt : elt -> Data.t
  val string_of_elt : elt -> string

  val take : t -> int option -> Data.t list
end

module type ListAppType = sig
  module ListRep : ListRepType
  val name    : string
  val compute : ListRep.t -> ListRep.t
  val trusted : ListRep.Data.t list -> ListRep.Data.t list
end

module Make_experiment ( ListApp : ListAppType ) = struct
  module Memotables = ListApp.ListRep.Memotables

  let name = ListApp.name

  let demand_list = ListApp.ListRep.take

  module T = Types
  module Stats = Statistics

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
    let random_new_elt_int () = (Params.n * 128) + (Random.int (Params.n * 32)) in

    let benchmark_demand name n roundi handle graphout l computation final_art initial_dcg_size =
      let input_art = l in
      let output_art = computation in

      let assert_correct_output changedesc changeposi changepos =
        let xs = ListApp.trusted (demand_list l None) in
        let ys = demand_list computation None in
        if ( xs = ys ) then
          Printf.fprintf stdout "Checked.\n%!"
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
          Printf.printf "%s: input:\t%s\n%!" msg (string_of_list (demand_list input_art None));
          Printf.printf "%s: output:\t%s\n%!" msg (string_of_list (demand_list output_art None));
        ) else ()
      in

      let do_delete_insert_interaction pos l' demand_count =
        let rem_msg elt =
          Printf.sprintf "di-delete %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt elt) pos demand_count in
        (* Delete element. *)
        let (elt,_), deleteStats = Stats.measure begin fun () ->
          let elt = ListApp.ListRep.delete_elt l' in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (rem_msg elt) ;
	        elt, (demand_list computation (Some demand_count))
        end
        in
        print_inout (rem_msg elt) ;
        if Params.Flags.check_output then assert_correct_output (rem_msg elt) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        let ins_msg elt =
          Printf.sprintf "di-insert %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt elt) pos demand_count in
        (* Reinsert element. *)
        let _, insertStats = Stats.measure begin fun () ->
          (* Demand list. *)
          ListApp.ListRep.insert_elt l' elt ;
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (ins_msg elt) ;
          demand_list computation (Some demand_count)
        end
        in
        print_inout (ins_msg elt) ;
        if Params.Flags.check_output then assert_correct_output (ins_msg elt) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        [("di-delete",deleteStats); ("di-insert",insertStats)]
      in

      let do_insert_delete_interaction pos l' demand_count =
        let new_elt = ListApp.ListRep.elt_of_int (random_new_elt_int ()) in
        let ins_msg elt =
          Printf.sprintf "id-insert %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt new_elt) pos demand_count in
        (* Insert a (pathological!) element. *)
        let _, insertStats = Stats.measure begin fun () ->
	        (* Demand list. *)
          ListApp.ListRep.insert_elt l' new_elt ;
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (ins_msg new_elt) ;
          demand_list computation (Some demand_count)
        end
        in
        print_inout (ins_msg new_elt) ;
        if Params.Flags.check_output then assert_correct_output (ins_msg new_elt) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        let rem_msg elt =
          Printf.sprintf "id-delete %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt elt) pos demand_count in
        (* Delete element. *)
        let (elt,_), deleteStats = Stats.measure begin fun () ->
          let elt = ListApp.ListRep.delete_elt l' in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (rem_msg elt) ;
          elt, (demand_list computation (Some demand_count))
        end
        in
        print_inout (rem_msg elt) ;
        if Params.Flags.check_output then assert_correct_output (rem_msg elt) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        [("id-insert",insertStats); ("id-delete",deleteStats)]
      in

      let do_replace_interaction pos l' demand_count =
        let new_int = (random_new_elt_int ()) in
        let new_elt = ListApp.ListRep.elt_of_int new_int in
        let replace_msg elt =
          Printf.sprintf "r-replace %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt new_elt) pos demand_count in
        (* Insert a (pathological!) element. *)
        let _, replaceStats = Stats.measure begin fun () ->
          let elt = ListApp.ListRep.delete_elt l' in
          let elt = ListApp.ListRep.elt_update elt new_int in
          let _ = ListApp.ListRep.insert_elt l' elt in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (replace_msg new_elt) ;
          demand_list computation (Some demand_count)
        end
        in
        print_inout (replace_msg new_elt) ;
        if Params.Flags.check_output then assert_correct_output (replace_msg new_elt) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        [("r-replace",replaceStats)]
      in

      let do_replace_replace_interaction pos l' demand_count =
        let new_int = (random_new_elt_int ()) in
        let new_elt = ListApp.ListRep.elt_of_int new_int in
        let replace_msg num elt =
          Printf.sprintf "rr-replace%d %s @ %d; demand %d"
            num (ListApp.ListRep.string_of_elt new_elt) pos demand_count in
        (* Insert a (pathological!) element. *)
        let old, replace1Stats = Stats.measure begin fun () ->
          let elt = ListApp.ListRep.delete_elt l' in
          let old = ListApp.ListRep.data_of_elt elt in
          let elt = ListApp.ListRep.elt_update elt new_int in
          let _ = ListApp.ListRep.insert_elt l' elt in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (replace_msg 1 new_elt) ;
          let _ = demand_list computation (Some demand_count) in
          old
        end in
        print_inout (replace_msg 1 new_elt) ;
        if Params.Flags.check_output then assert_correct_output (replace_msg 1 new_elt) pos l' ;

        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;

        let _, replace2Stats = Stats.measure begin fun () ->
          let elt = ListApp.ListRep.delete_elt l' in
          let elt = ListApp.ListRep.elt_update elt old in
          let _ = ListApp.ListRep.insert_elt l' elt in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (replace_msg 2 new_elt) ;
          demand_list computation (Some demand_count)
        end in
        print_inout (replace_msg 2 new_elt) ;
        if Params.Flags.check_output then assert_correct_output (replace_msg 2 new_elt) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;

        [("rr-replace1",replace1Stats); ("rr-replace2",replace2Stats)]
      in

      let do_swap_swap_interaction pos l' demand_count =
        let msg elt1 elt2 =
          Printf.sprintf "ss-swap1 %s, %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt elt1) (ListApp.ListRep.string_of_elt elt2) pos demand_count in
        let (elt1,elt2), swap1Stats = Stats.measure begin fun () ->
          let elt1 = ListApp.ListRep.delete_elt l' in
          let elt2 = ListApp.ListRep.delete_elt l' in
          let elt1_data = ListApp.ListRep.data_of_elt elt1 in
          let elt2_data = ListApp.ListRep.data_of_elt elt2 in
          let elt1 = ListApp.ListRep.elt_update elt1 elt2_data in
          let elt2 = ListApp.ListRep.elt_update elt2 elt1_data in
          let _ = ListApp.ListRep.insert_elt l' elt2 in
          let _ = ListApp.ListRep.insert_elt l' elt1 in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (msg elt1 elt2) ;
          ignore ( demand_list computation (Some demand_count) );
          elt1, elt2
        end in
        print_inout (msg elt1 elt2) ;
        if Params.Flags.check_output then assert_correct_output (msg elt1 elt2) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        let msg elt1 elt2 =
          Printf.sprintf "ss-swap2 %s, %s @ %d; demand %d"
            (ListApp.ListRep.string_of_elt elt1) (ListApp.ListRep.string_of_elt elt2) pos demand_count in
        let (elt1,elt2), swap2Stats = Stats.measure begin fun () ->
          let elt1 = ListApp.ListRep.delete_elt l' in
          let elt2 = ListApp.ListRep.delete_elt l' in
          let elt1_data = ListApp.ListRep.data_of_elt elt1 in
          let elt2_data = ListApp.ListRep.data_of_elt elt2 in
          let elt1 = ListApp.ListRep.elt_update elt1 elt2_data in
          let elt2 = ListApp.ListRep.elt_update elt2 elt1_data in
          let _ = ListApp.ListRep.insert_elt l' elt2 in
          let _ = ListApp.ListRep.insert_elt l' elt1 in
          if Params.Flags.print_changes then Printf.fprintf stdout "%s: %s\n%!" name (msg elt1 elt2) ;
          ignore ( demand_list computation (Some demand_count) ) ;
          elt1, elt2
        end
        in
        print_inout (msg elt1 elt2) ;
        if Params.Flags.check_output then assert_correct_output (msg elt1 elt2) pos l' ;
        if Params.Flags.print_changes then begin
          (*Memotables.print_stats stdout ;*)
        end ;
        [("ss-swap1",swap1Stats); ("ss-swap2",swap2Stats)]
      in

      let do_move_interaction pos mid_art demand_count =
        let msg () =
          Printf.sprintf "mv-move1 %d; demand %d" pos demand_count
        in
        let _, move1Stats = Stats.measure begin fun () ->
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
          (*Memotables.print_stats stdout ;*)
        end ;
        let msg () =
          Printf.sprintf "mv-move2 %d; demand %d" pos demand_count
        in
        let _, move2Stats = Stats.measure begin fun () ->
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
          (*Memotables.print_stats stdout ;*)
        end ;
        [("mv-move1",move1Stats); ("mv-move2",move2Stats)]
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
            let stats_list6 = if List.mem "mv" Params.interactions then do_move_interaction            change_pos_idx change_pos_inp demand_count else [] in
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
    (*Memotables.print_stats stdout ;*)

    (* make file header *)
    stats_labels_print handle;

    (* Do the run(s), for the number of lists requested: *)
    for i = 0 to Params.num_lists - 1 do
      Random.init (Params.sample_num + i); (* Fix seed. *)
      let raw_input = gen_list Params.n [] in
      let input = ListApp.ListRep.of_list raw_input Params.granularity in
      let final_art = ListApp.ListRep.last input in
      if Params.Flags.print_inout then begin
        Printf.printf "%d: Initial input:\t%s\n\n%!" i (ListApp.ListRep.string_of_list input);
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
        Printf.printf "%d: Initial output:\t%s\n%!" i (ListApp.ListRep.string_of_list output);
      end;
      Printf.printf "\n----------------------------------------------------------------------------------\n%!" ;
      Printf.printf "%s: Initial run:\ttime:%.4fs (unit cost %d)\n%!" line_prefix s.Stats.time s.Stats.unit_cost ;
      Printf.printf "%s: Initial run:\t{dirty:%d; clean:%d; eval:%d; create:%d; tables:%d}\n%!"
        line_prefix s.Stats.dirty s.Stats.clean s.Stats.evaluate s.Stats.create s.Stats.tables ;
      (*Memotables.print_stats stdout ;*)
      let (_,_) =
        if Params.Flags.print_inout then
          (string_of_list (demand_list input None)),
          (string_of_list (demand_list output None))
        else
          ("<omitted>","<omitted>")
      in
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
           | exn -> raise exn) ;
        let interact_end_time = Unix.gettimeofday () in
        Printf.printf "%s: Iteraction time: %f (sec)\n%!" line_prefix (interact_end_time -. interact_begin_time) ;
      );
      (*Memotables.print_stats stdout ;*)
    done;
    close_out handle;
  ) (* run *)
end (* Make_experiment *)


(* ------------------------------------------------------------------------------- *)

module Key = Name
module Int = Types.Int

module SpreadTreeRep
  (ArtLib : ArtLib.S)
  (* : ListRepType *) =
struct
  module ArtLib = ArtLib
  module St = SpreadTree.Make(ArtLib)(Key)(Int)
  module Seq = SpreadTree.SeqWrap(ArtLib)(Key)(Int)(St)
  module Data = Int
  module Memotables = ArtLib.Memotables
  module KvMap = SpreadTree.KvMapWrap(ArtLib)(Key)(Int)(Int)(St)

  type t   = St.List.Art.t
  type elt = Data.t

  let of_list x gran =
    Seq.mut_elts_of_list
      (* ~c=false *) (* true/default: Cons-Name-Art, false: Name-Cons-Art *)
      (Key.gensym())
      (List.map (fun x -> (x, Key.gensym())) x)
      fst snd gran

  let next x = Seq.next_art (St.List.Art.force x)

  let take x count =
    ArtLib.sac_refresh () ;
    Seq.take (St.List.Art.force x) count

  let delete_elt list =
    let h,_ = Seq.delete_elt list in
    h

  let last list =
    Seq.get_or_create_final_art list

  (* needed to break the higher level abstraction to swap whole chunks of a list *)
  let set_art art data = Seq.LArt.set art data
  let force_art art = Seq.LArt.force art

  let insert_elt list h =
    Seq.insert_elt list h None

  let string_of_list art = Seq.simple_full_string (`Art art)

  let elt_of_int h = h
  let elt_update x y = y
  let data_of_elt x = x
  let string_of_elt x = Printf.sprintf "%d" x
end

module RepOfSpreadTree
  (ArtLib : ArtLib.S)
  (Name : Name.S)
  (St : SpreadTree.S with type  elt = int
                      and type name = Name.t)
  (* : ListRepType *) =
struct
  module Seq = SpreadTree.SeqWrap(ArtLib)(Name)(Int)(St)

  module Memotables = ArtLib.Memotables
  module KvMap = SpreadTree.KvMapWrap(ArtLib)(Name)(Int)(Int)(St)

  type t      = KvMap.KeySt.List.Art.t
  module Data = Int

  type elt = Data.t

  let of_list x =
    KvMap.KeySeq.mut_elts_of_list (Name.gensym())
      (List.map (fun x -> (x, Name.gensym())) x)
      fst snd

  let next x = KvMap.KeySeq.next_art (KvMap.KeySt.List.Art.force x)
  let last x = failwith "unimplemented"

  let take x count =
    ArtLib.sac_refresh () ;
    KvMap.KeySeq.take (KvMap.KeySt.List.Art.force x) count

  let delete_elt list =
    let h,_ = KvMap.KeySeq.delete_elt list in
    h

  let insert_elt list h =
    KvMap.KeySeq.insert_elt list h None

  let force_art art = failwith "unimplemented"
  let set_art art data = failwith "unimplemented"

  let string_of_list x = "not implemented"

  let elt_of_int h = h
  let elt_update x y = y
  let data_of_elt x = x
  let string_of_elt x = Printf.sprintf "%d" x
end

(* ------------------------------------------------------------------------------- *)

module Linear = struct
  module List_filter
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_filter_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let filter_even x = x mod 2 = 0
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
      let filter = ListRep.Seq.list_filter nm1 filter_even in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        filter (ListRep.St.List.Art.force inp)
      )
    let trusted = List.filter filter_even
  end

  module List_eager_filter
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_eager_filter_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let filter_even x = x mod 2 = 0
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
      let filter = ListRep.Seq.list_eager_filter nm1 filter_even in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        filter (ListRep.St.List.Art.force inp)
      )
    let trusted = List.filter filter_even
  end

  module List_map
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_map_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let almost_triple x = x * 2 - 1
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
      let map = ListRep.Seq.list_map nm1 almost_triple in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        map (ListRep.St.List.Art.force inp)
      )
    let trusted = List.map almost_triple
  end

  module List_eager_map
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_eager_map_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let almost_triple x = x * 2 - 1
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
      let map = ListRep.Seq.list_eager_map nm1 almost_triple in
      ListRep.St.List.Art.thunk nm2 ( fun () -> 
        map (ListRep.St.List.Art.force inp)
      )
    let trusted = List.map almost_triple
  end

  module List_map_paired
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_map_paired_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
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
  end
    
  module List_unique
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_unique_" ^ N.name
    module ListRep = SpreadTreeRep (AL )
    module U = Unique.Make(AL)(Name)(Int)(ListRep.St)
    let unique = U.list_unique (0,1)
    let compute inp =
      ListRep.St.List.Art.thunk (Name.of_string "List_unique.compute")
                                ( fun () -> unique                                  
                                    ( ListRep.St.List.Art.force inp ) )
    module S = Set.Make(struct type t = int let compare = compare end)
    let trusted elts =
      let rec loop elts set result =
        match elts with
        | [] -> List.rev result
        | elt::elts ->
           loop elts (S.add elt set) ((if S.mem elt set then 1 else 0)::result)
      in loop elts S.empty []
  end

end

module Reverse = struct

  module Rope_reverse
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "Rope_reverse_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = (Key.gensym ()) in
      let rev = ListRep.Seq.rope_reverse in
      let to_rope = ListRep.Seq.rope_of_list in
      let to_list = ListRep.Seq.list_of_rope in
      let force = ListRep.St.List.Art.force in
      ListRep.St.List.Art.thunk nm ( fun () ->
        to_list (rev (to_rope (force inp))) `Nil
      )
    let trusted = List.rev
  end

  module List_reverse
    (N : sig val name : string end)
    (AL : ArtLib.S) =
  struct
    let name = "List_reverse_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = (Key.gensym ()) in
      let rev = ListRep.Seq.list_reverse_balanced in
      let force = ListRep.St.List.Art.force in
      ListRep.St.List.Art.thunk nm ( fun () ->
                                     let res = rev(force inp) `Nil in
                                     (* Printf.printf "... Computed: %s\n" (ListRep.St.List.Data.string res) ; *)
                                     res
                                   )
    let trusted = List.rev
  end
end

module Mergesorts = struct

  module Rope_mergesort
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "Rope_mergesort_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm = Key.fork (Key.gensym ()) in
      let mergesort = ListRep.Seq.list_to_rope_mergesort
                        (fst nm) int_compare in
      ListRep.St.List.Art.thunk (snd nm) ( fun () ->
        mergesort (ListRep.St.List.Art.force inp)
      )
    let trusted = List.sort int_compare
  end

end

module Median = struct

  module Rope_median
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "Rope_median_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
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
  end

  module Rope_center
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "Rope_center_" ^ N.name
    let int_compare : int -> int -> int = Pervasives.compare
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let nm1 = Key.gensym() in
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
  end
end

module Pointcloud = struct
  
  module List_Quickhull
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "Quickhull_" ^ N.name
    module ListRep = SpreadTreeRep(AL)
    module QH = Quickhull.Make(AL)(Name)(ListRep.St)
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
      let quickhull = QH.list_quickhull in
      ListRep.St.List.Art.thunk nm1 ( fun () -> 
        quickhull nm2 (ListRep.St.List.Art.force inp)
      )
    let trusted = Quickhull.list_quickhull
  end

  module List_Max_Distance
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "List_max_dist_" ^ N.name
    module ListRep = SpreadTreeRep(AL)
    module QH = Quickhull.Make(AL)(Name)(ListRep.St)
    let compute inp =
      let nm1, nm2 = Key.fork (Key.gensym()) in
      let list_max = QH.list_max_dist in
      ListRep.St.List.Art.thunk nm1 ( fun () -> 
        list_max nm2 (ListRep.St.List.Art.force inp)
      )
    let trusted xs = [int_of_float @@ Quickhull.single_list_max_dist xs]
  end

end

module Iteration = struct
  module Rope_iter
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "Rope_iter_" ^ N.name
    module ListRep = SpreadTreeRep ( AL )
    let compute inp =
      let rope_of_list = ListRep.Seq.rope_of_list in
      let list_of_rope = ListRep.Seq.list_of_rope in
      ListRep.St.List.Art.thunk (Key.gensym ()) (
        fun () ->
          let rope = rope_of_list (ListRep.St.List.Art.force inp) in
          list_of_rope rope `Nil
      )
    let trusted = List.map (fun x -> x)
  end
end

module List_transf = struct
  let mapf x = int_of_float ((log (1. +. float_of_int x)) +. log 1.5)
  let filterf x = ((x mod 2) = 0)

end

module Reduction = struct

  module Rope_reduce_monoid
    ( N : sig val name : string end )
    ( AL : ArtLib.S )
    ( Monoid : sig val name : string
                   val id_elt : int
                   val bin : int -> int -> int end ) =
  struct
    let name = "Rope_" ^ Monoid.name ^ "_" ^ N.name
    module ListRep = SpreadTreeRep ( AL )
    let min_of_ints x y = (
      incr Statistics.Counts.unit_cost ;
      Monoid.bin x y
    )
    let compute inp =
      let rope_reduce = ListRep.Seq.rope_reduce (Key.gensym()) min_of_ints in
      ListRep.St.List.Art.thunk (Key.gensym ()) ( fun () ->
          let rope = ListRep.Seq.rope_of_list (`Art inp) in
          match rope_reduce rope with
          | None -> `Nil
          | Some x -> `Cons(x,`Nil)
      )
    let trusted x = match x with
      | [] -> []
      | _ -> [ List.fold_left Monoid.bin Monoid.id_elt x ]
  end

  module Rope_min
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    include Rope_reduce_monoid(N)(AL)
      (struct
        let name = "min"
        let id_elt = max_int
        let bin x y = if x < y then x else y
       end)
  end

  module Rope_sum
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    include Rope_reduce_monoid(N)(AL)
      (struct
        let name = "sum"
        let id_elt = 0
        let bin x y = x + y
       end)
  end

  module AVL_of_rope
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "AVL_of_rope_" ^ N.name
    module St = SpreadTree.Make(AL)(Key)(Int)
    module ListRep = RepOfSpreadTree(AL)(Key)(St)
    let compute inp =
      let rope_of_list = ListRep.KvMap.KeySeq.rope_of_list in
      let name = (Key.gensym()) in
      ListRep.KvMap.KeySt.List.Art.thunk (Key.gensym ()) ( fun () ->
        let rope = rope_of_list (`Art inp) in
        let avl = ListRep.KvMap.avl_tree_of_rope name rope (`Leaf `Nil) in
        (ListRep.KvMap.KeySeq.list_of_tree avl `Nil)
      )
    let trusted x =
      let module S = Set.Make(struct type t = int let compare = Pervasives.compare end) in
      let set = List.fold_left (fun set elt -> S.add elt set) S.empty x in
      (S.elements set)
  end

end

(*
module Graph = struct

  module Iter
    ( N : sig val name : string end )
    ( AL : ArtLib.S ) =
  struct
    let name = "Graph_iter_" ^ N.name
    module St = Adapton_structures.SpreadTree.MakeSpreadTree(AL)(Key)(Int)
    module ListRep = RepOfSpreadTree ( St )

    module G = Adapton_structures.Graph.Make(Key)(AL)
                                            (AdaptonTypes.Int)
                                            (AdaptonTypes.Int)
                                            (AdaptonTypes.Int)
    let compute inp =
      failwith "TODO"

    let trusted x =
      failwith "TODO"

  end

end
*)
                     
(* ----------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------------------------- *)

module Engines = struct
  module Nominal_name = Engine.Make(
    struct
      include Engine.Default_params
    end )
  module Nominal_arg = Engine.Make(
    struct
      include Engine.Default_params
      let disable_names = true
    end )
  module Nominal_arggen = Engine.Make(
    struct
      include Engine.Default_params
      let disable_names = true
      let generative_ids = true
    end )
  module Nominal_nocheck = Engine.Make(
    struct
      include Engine.Default_params
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
    module ListApp_name = Linear.List_filter(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Linear.List_filter(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Linear.List_filter(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_filter(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_filter(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_filter(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_filter(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_eager_filter = struct
    module ListApp_name = Linear.List_eager_filter(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Linear.List_eager_filter(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Linear.List_eager_filter(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_eager_filter(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_eager_filter(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_eager_filter(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_eager_filter(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_map = struct
    module ListApp_name = Linear.List_map(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Linear.List_map(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Linear.List_map(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_map(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_map(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_map(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_map(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_eager_map = struct
    module ListApp_name = Linear.List_eager_map(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Linear.List_eager_map(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Linear.List_eager_map(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_eager_map(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_eager_map(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_eager_map(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_eager_map(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_map_paired = struct
    module ListApp_name = Linear.List_map_paired(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Linear.List_map_paired(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Linear.List_map_paired(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_map_paired(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_map_paired(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_map_paired(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
  end

  module List_reverse = struct
    module ListApp_name = Reverse.List_reverse(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arggen = Reverse.List_reverse(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Reverse.List_reverse(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reverse.List_reverse(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_reverse = struct
    module ListApp_name = Reverse.Rope_reverse(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arggen = Reverse.Rope_reverse(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Reverse.Rope_reverse(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reverse.Rope_reverse(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_unique = struct
    module ListApp_name = Linear.List_unique(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Linear.List_unique(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Linear.List_unique(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Linear.List_unique(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Linear.List_unique(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Linear.List_unique(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Linear.List_unique(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_mergesort = struct
    module ListApp_name = Mergesorts.Rope_mergesort(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Mergesorts.Rope_mergesort(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Mergesorts.Rope_mergesort(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Mergesorts.Rope_mergesort(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Mergesorts.Rope_mergesort(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_lazy_noninc = Mergesorts.Rope_mergesort(struct let name = "lazynoninc" end)(LazyNonInc.ArtLib)
    module ListApp_sac = Mergesorts.Rope_mergesort(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_median = struct
    module ListApp_name = Median.Rope_median(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Median.Rope_median(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Median.Rope_median(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Median.Rope_median(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Median.Rope_median(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_sac = Median.Rope_median(struct let name = "sac" end)(Sac.ArtLib)
  end

  (* to test the median, this finds center without sorting first *)
  module Rope_center = struct
    module ListApp_name = Median.Rope_center(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Median.Rope_center(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Median.Rope_center(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Median.Rope_center(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_eager_noninc = Median.Rope_center(struct let name = "eagernoninc" end)(EagerNonInc.ArtLib)
    module ListApp_sac = Median.Rope_center(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Quickhull = struct
    module ListApp_name = Pointcloud.List_Quickhull(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arggen = Pointcloud.List_Quickhull(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Pointcloud.List_Quickhull(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Pointcloud.List_Quickhull(struct let name = "sac" end)(Sac.ArtLib)
  end

  module List_Max_Distance = struct
    module ListApp_name = Pointcloud.List_Max_Distance(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arggen = Pointcloud.List_Max_Distance(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Pointcloud.List_Max_Distance(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Pointcloud.List_Max_Distance(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_iter = struct
    module ListApp_name = Iteration.Rope_iter(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Iteration.Rope_iter(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Iteration.Rope_iter(struct let name = "arggen" end)(Nominal_name.ArtLib)
    module ListApp_sac = Iteration.Rope_iter(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_min = struct
    module ListApp_name = Reduction.Rope_min(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Reduction.Rope_min(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Reduction.Rope_min(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Reduction.Rope_min(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reduction.Rope_min(struct let name = "sac" end)(Sac.ArtLib)
  end

  module Rope_sum = struct
    module ListApp_name = Reduction.Rope_sum(struct let name = "name" end)(Nominal_name.ArtLib)
    module ListApp_arg = Reduction.Rope_sum(struct let name = "arg" end)(Nominal_arg.ArtLib)
    module ListApp_arggen = Reduction.Rope_sum(struct let name = "arggen" end)(Nominal_arggen.ArtLib)
    module ListApp_lazy_recalc = Reduction.Rope_sum(struct let name = "lazyrecalc" end)(LazyRecalc.ArtLib)
    module ListApp_sac = Reduction.Rope_sum(struct let name = "sac" end)(Sac.ArtLib)
  end

(* older or unfinished experiments

  module AVL_name = Reduction.AVL_of_rope(struct let name = "grifola_name" end)(Nominal_name.ArtLib)
  module AVL_arggen = Reduction.AVL_of_rope(struct let name = "grifola_arggen" end)(Nominal_arggen.ArtLib)
  module AVL_lazy_recalc = Reduction.AVL_of_rope(struct let name = "lazy_recalc" end)(LazyRecalc.ArtLib)

  module Graph_iter_name = Graph.Iter(struct let name = "name" end)(Nominal_name.ArtLib)
*)

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

  (* List eager filter *)
  (module Experiments.List_eager_filter.ListApp_name         : ListAppType) ;
  (module Experiments.List_eager_filter.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_eager_filter.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_eager_filter.ListApp_lazy_noninc  : ListAppType) ;
  (module Experiments.List_eager_filter.ListApp_eager_noninc  : ListAppType) ;
  (module Experiments.List_eager_filter.ListApp_sac             : ListAppType) ;

  (* List map *)
  (module Experiments.List_map.ListApp_name         : ListAppType) ;
  (module Experiments.List_map.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_map.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_map.ListApp_sac          : ListAppType) ;

  (* List eager map *)
  (module Experiments.List_eager_map.ListApp_name         : ListAppType) ;
  (module Experiments.List_eager_map.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_eager_map.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_eager_map.ListApp_lazy_noninc  : ListAppType) ;
  (module Experiments.List_eager_map.ListApp_eager_noninc  : ListAppType) ;
  (module Experiments.List_eager_map.ListApp_sac          : ListAppType) ;

  (* List map paired *)
    (* checking this will fail when list lengths are
    not even numbers, like after insert or delete *)
  (module Experiments.List_map_paired.ListApp_name         : ListAppType) ;
  (module Experiments.List_map_paired.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_map_paired.ListApp_lazy_recalc  : ListAppType) ;

  (* List unique *)
  (module Experiments.List_unique.ListApp_name         : ListAppType) ;
  (module Experiments.List_unique.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_unique.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_unique.ListApp_sac          : ListAppType) ;

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

  (* List_Max_Distance *)
  (module Experiments.List_Max_Distance.ListApp_name         : ListAppType) ;
  (module Experiments.List_Max_Distance.ListApp_arggen       : ListAppType) ;
  (module Experiments.List_Max_Distance.ListApp_lazy_recalc  : ListAppType) ;
  (module Experiments.List_Max_Distance.ListApp_sac          : ListAppType) ;

(* older or unfinished experiments

  (* Benchmarks for overhead comparison: *)
  (module Experiments.AVL_name                         : ListAppType) ;
  (module Experiments.AVL_arggen                       : ListAppType) ;
  (module Experiments.AVL_lazy_recalc                  : ListAppType) ;

  (module Experiments.Graph_iter_name : ListAppType) ;

*)

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
  let interactions = [ "di"; "id"; "ss"; "rr"; "r"]
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
(*    ("--num-lists",   Arg.Int   (fun n -> num_lists_ := n),   " number of lists to try"); *)
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
          Printf.eprintf "Warning: Couldn't find a unique experiment with the requested name `%s'\n%!" name_req ;
          if List.length matches > 0 then (
            Printf.eprintf "The experiment name `%s' is not unique: Found %d matches total, but expected exactly one.\n%!" name_req (List.length matches);
          ) ;
          Printf.eprintf "  Try one of these:\n%!" ;
          List.iter (fun name -> Printf.eprintf "   %s\n%!" name) names

        | [req_name] -> experiment_ := req_name

      )), "choose the experiment by name");

    ("--0",   Arg.Unit  (fun () -> interactions_ := []),                     " clear interaction list");
    ("--di",  Arg.Unit  (fun () -> interactions_ := "di" :: !interactions_), " add interaction: delete, insert");
    ("--id",  Arg.Unit  (fun () -> interactions_ := "id" :: !interactions_), " add interaction: insert, delete");
    ("--r",   Arg.Unit  (fun () -> interactions_ := "r"  :: !interactions_), " add interaction: replace");
    ("--rr",  Arg.Unit  (fun () -> interactions_ := "rr" :: !interactions_), " add interaction: replace, replace");
    ("--ss",  Arg.Unit  (fun () -> interactions_ := "ss" :: !interactions_), " add interaction: swap, swap");
    ("--mv",  Arg.Unit  (fun () -> interactions_ := "mv" :: !interactions_), " add interaction: move front of list to back, return");

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
