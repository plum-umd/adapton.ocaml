(** Adapton performance statistics and measurement function. *)

module Counts = struct
    let create = ref 0
    let hit = ref 0
    let miss = ref 0
    let update = ref 0
    let dirty = ref 0
    let clean = ref 0
    let evaluate = ref 0
    let tables = ref 0
    let evict = ref 0
    let destroy = ref 0

    (* XXX -- always distinguish these cases; remove general case above. *)
    let destroy_refc = ref 0
    let destroy_evict = ref 0

    let unit_cost = ref 0

    let reset () =
      create := 0 ; hit := 0 ; miss := 0 ; update := 0 ; dirty := 0 ;
      clean := 0 ; evaluate := 0 ; tables := 0 ; evict := 0 ; destroy := 0 ;
      destroy_refc := 0 ; destroy_evict := 0 ; unit_cost := 0
end

(* Count operations that compute information about node equivalence classes *)
(* Changing the representations of nodes requires considering how important different computations over equivalence classes are. *)
module Count_eq = struct
  let id            = ref 0 (* To support tableless uneval'd thunks, need this to be zero (!) *)
  let hash          = ref 0 (* This can be arbitrarily high, since we can cache hashes in the repr. *)
  let mut_cell      = ref 0 (* Cheap: test for physical equality. *)
  let one_pre_force = ref 0 (* Want this to be low for non-nominal thunks with long "thunk-argument chains".
                               Nominal thunks can always be compared cheaply, due to keys. *)
  let post_force    = ref 0 (* Both are evaluated. Cheap for nominal and non-nominal: test for physical equality. *)
  let one_obsolete  = ref 0
  let both_obsolete = ref 0

  let reset () =
    id := 0 ; hash := 0 ; mut_cell := 0 ; one_pre_force := 0 ;
    post_force := 0 ; one_obsolete := 0 ; both_obsolete := 0
end

let fprintf_count_eq out =
  Printf.fprintf out "Count_eq { id=%d, hash=%d, equals={ mut_cell=%d, one_pre_force=%d, post_force=%d, one_obsolete=%d, both_obsolete=%d } }\n"
    (!Count_eq.id)
    (!Count_eq.hash)
    (!Count_eq.mut_cell)
    (!Count_eq.one_pre_force)
    (!Count_eq.post_force)
    (!Count_eq.one_obsolete)
    (!Count_eq.both_obsolete)

let word_size = Sys.word_size / 8

let word_bytes words = word_size * words

let word_megabytes words = float_of_int (word_bytes words) /. 1048576.

let get_time = Unix.gettimeofday

let get_heap_stack () = Gc.(let s = quick_stat () in ( s.heap_words, s.stack_size ))

(**/**) (* helper values/functions *)
let heap_stacks = ref []
let top_stack = ref 0
let _ = Gc.create_alarm begin fun () ->
    let heap, stack = Gc.(let s = quick_stat () in ( s.heap_words, s.stack_size )) in
    List.iter (fun ( h, s ) -> h := max heap !h; s := max stack !s) !heap_stacks;
    top_stack := max stack !top_stack
end
(**/**)

let get_top_heap_stack () = ( Gc.((quick_stat ()).top_heap_words), !top_stack )

type t = {
    time : float; (** Elapsed time in seconds. *)
    heap : int; (** Max heap delta in bytes. *)
    stack : int; (** Max stack delta in bytes. *)
    create : int; (** Thunks created. *)
    hit : int; (** Thunks memo-hit. *)
    miss : int; (** Thunks memo-missed. *)
    update : int; (** Thunks updated. *)
    evaluate : int; (** Thunks re-evaluated. *)
    dirty : int; (** For {!Adapton}, dependencies to be checked; for {!EagerTotalOrder}, thunks scheduled for re-evaluation. *)
    clean : int; (** For {!Adapton}, dependencies checked clean; for {!EagerTotalOrder}, thunks unscheduled for re-evaluation (due to invalidation). *)
    tables : int;
    evict : int;
    destroy : int;
    unit_cost : int;
    destroy_refc : int;
    destroy_evict : int;
}

let add s s' = {
    time=s.time +. s'.time;
    heap=s.heap + s'.heap;
    stack=s.stack + s'.stack;
    create=s.create + s'.create;
    hit=s.hit + s'.hit;
    miss=s.miss + s'.miss;
    update=s.update + s'.update;
    evaluate=s.evaluate + s'.evaluate;
    dirty=s.dirty + s'.dirty;
    clean=s.clean +s'.clean;
    tables=s.tables + s'.tables;
    evict=s.evict + s'.evict;
    destroy=s.destroy + s'.destroy;
    unit_cost=s.unit_cost + s'.unit_cost;
    destroy_refc=s.destroy_refc + s'.destroy_refc;
    destroy_evict=s.destroy_evict + s'.destroy_evict;
}

let measure f =
    let heap' = ref 0 in
    let stack' = ref 0 in
    let create = !Counts.create in
    let hit = !Counts.hit in
    let miss = !Counts.miss in
    let update = !Counts.update in
    let dirty = !Counts.dirty in
    let clean = !Counts.clean in
    let evaluate = !Counts.evaluate in
    let tables = !Counts.tables in
    let evict = !Counts.evict in
    let destroy = !Counts.destroy in
    let unit_cost = !Counts.unit_cost in
    let destroy_refc = !Counts.destroy_refc in
    let destroy_evict = !Counts.destroy_evict in
    let old_heap_stacks = !heap_stacks in
    heap_stacks := ( heap', stack' )::old_heap_stacks;
    let heap, stack = get_heap_stack () in
    heap' := max heap !heap';
    stack' := max stack !stack';
    let time = get_time () in
    let x = f () in
    let time = get_time () -. time in
    heap_stacks := old_heap_stacks;
    let measurement = {
        time;
        heap=word_bytes (!heap' - heap);
        stack=word_bytes (!stack' - stack);
        create=(!Counts.create - create);
        hit=(!Counts.hit - hit);
        miss=(!Counts.miss - miss);
        update=(!Counts.update - update);
        evaluate=(!Counts.evaluate - evaluate);
        dirty=(!Counts.dirty - dirty);
        clean=(!Counts.clean - clean);
        tables=(!Counts.tables - tables);
        evict=(!Counts.evict - evict);
        destroy=(!Counts.destroy - destroy);
        unit_cost=(!Counts.unit_cost - unit_cost);
        destroy_refc=(!Counts.destroy_refc - destroy_refc);
        destroy_evict=(!Counts.destroy_evict - destroy_evict);
    } in
    ( x, measurement )
