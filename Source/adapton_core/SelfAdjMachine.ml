(** Self-Adjusting Machine.

    * Based on Yit's "SAC Library" implementation, from Adapton / PLDI 2014.

    * Adapted to perform destination-passing-style transformation,
      a la self-adjusting machines (OOPSLA 2011, Hammer's Diss 2012).

 **)

exception Missing_nominal_features

open Primitives
module type ResultType = DatType
module Statistics = AdaptonStatistics

(** Types and operations common to EagerTotalOrder thunks containing any type. *)
module T = struct
    (** Abstract type identifying this module. *)
    type atype

    (** EagerTotalOrder thunks containing ['a]. *)
    type 'a thunk = { (* 3 + 16 = 19 words *)
        id : int;
        mutable value : 'a;
        meta : meta;
    }
    (**/**) (* auxiliary types *)
   and meta = { (* 5 + 5 + 5 = 15 words (not including closures of evaluate and unmemo as well as WeakDyn.t) *)
        mid : int;
        mutable enqueued : bool;
        mutable evaluate : unit -> unit;
        mutable unmemo : unit -> unit;
        mutable start_timestamp : TotalOrder.t; (* for const thunks, {start,end}_timestamp == TotalOrder.null and evaluate == nop *)
        mutable end_timestamp : TotalOrder.t; (* while evaluating non-const thunk, end_timestamp == TotalOrder.null *)
        dependents : meta WeakDyn.t;
        (* dependents doesn't have to be a set since it is cleared and dependents are immediately re-evaluated and re-added if updated;
            also, start_timestamp invalidators should provide strong references to dependencies to prevent the GC from breaking the dependents graph *)
    }
    (**/**)


    (** This module implements incremental thunks. *)
    let is_incremental = true

    (** This module implements eager values. *)
    let is_lazy = false


    (**/**) (* internal state and helper functions *)

    (* use a priority set because, although the size is usually quite small, duplicate insertions occur frequently *)
    module PriorityQueue = PrioritySet.Make (struct
        type t = meta
        let compare meta meta' = TotalOrder.compare meta.start_timestamp meta'.start_timestamp
    end)

    let eager_id_counter = AdaptonTypes.Counter.make 0
    let eager_stack = ref []
    let eager_queue = PriorityQueue.create ()
    let eager_start = TotalOrder.create ()
    let eager_now = ref eager_start
    let eager_finger = ref eager_start

    let add_timestamp () =
        let timestamp = TotalOrder.add_next !eager_now in
        Printf.printf "... add_timestamp: %d\n%!" (TotalOrder.id timestamp);
        eager_now := timestamp;
        timestamp

    let unqueue meta =
        if PriorityQueue.remove eager_queue meta then
            incr Statistics.Counts.clean

    let dequeue () =
      let m = PriorityQueue.pop eager_queue in
      assert( m.enqueued );
      m.enqueued <- false;
      m

    let enqueue_dependents dependents =
      WeakDyn.fold (
          fun d () ->
          if TotalOrder.is_valid d.start_timestamp then (
            if d.enqueued then
              Printf.printf "... already enqueued: dependent %d\n%!" (TotalOrder.id d.start_timestamp)
            else (
              Printf.printf "... enqueuing dependent %d\n%!" (TotalOrder.id d.start_timestamp) ;
              d.enqueued <- true ;
              if PriorityQueue.add eager_queue d then
                incr Statistics.Counts.dirty
            ))) dependents ()
      (* WeakDyn.clear dependents (* XXX *) (* ??? *) *)

    (**/**)


    (** Return the id of an EagerTotalOrder thunk. *)
    let id m = (Some m.id)

    (** Compute the hash value of an EagerTotalOrder thunk. *)
    let hash seed m = Hashtbl.seeded_hash seed m.id

    (** Compute whether two EagerTotalOrder thunks are equal. *)
    let equal = (==)

    (** Debugging string *)
    let string m = "&"^(string_of_int m.id)

    let sanitize m = m

    (** Recompute EagerTotalOrder thunks if necessary. *)
    let refresh_until end_time =
        let rec refresh () =
          match PriorityQueue.top eager_queue with
          | None -> ()
          | Some next -> (
            if TotalOrder.is_valid next.start_timestamp then (
              if (match end_time with
                    None -> true |
                    Some end_time -> TotalOrder.compare next.end_timestamp end_time <= 0 )
              then (
                let meta = dequeue () in
                assert ( match end_time with | None -> true | Some end_time -> TotalOrder.compare meta.end_timestamp end_time <= 0 ) ;
                eager_now := meta.start_timestamp;
                eager_finger := meta.end_timestamp;
                meta.evaluate ();
                TotalOrder.splice ~db:"refresh_until" !eager_now meta.end_timestamp;
                refresh ()
              )
              else (
                Printf.printf "... WARNING: refresh is stopping with non-empty queue. Top is: (%d, %d)\n%!"
                              (TotalOrder.id next.start_timestamp) (TotalOrder.id next.end_timestamp)
              ))
            else (
              Printf.printf "... WARNING: XXX refresh is skipping invalid timestamp.\n%!" ;
              let meta = dequeue () in
              assert( not (TotalOrder.is_valid meta.start_timestamp) );
            ))
        in
        let old_finger = !eager_finger in
        refresh () ;
        eager_finger := old_finger

    (** Recompute EagerTotalOrder thunks if necessary. *)
    let refresh () =
        let condition = (not (PriorityQueue.top eager_queue = None)) in
        if condition then Printf.printf ">>> BEGIN: global refresh:\n%!" ;
        if condition then (TotalOrder.iter eager_start (fun ts -> Printf.printf "... iter-dump: %d\n%!" (TotalOrder.id ts))) ;
        let last_now = !eager_now in
        (
          try
            (refresh_until None)
          with PriorityQueue.Empty ->
            eager_now := last_now;
            eager_finger := eager_start
        );
        if condition then Printf.printf "<<< END: global refresh.\n%!"

    let flush () = () (* Flushing is a no-op here: Flushing happens internally during change propagation. *)

    let make_dependency_edge m =
      (* add dependency to caller *)
      match !eager_stack with
      | dependent::_ -> WeakDyn.add m.meta.dependents dependent
      | [] ->
         (* Force is occuring at the outer layer. *)
         (* Make sure that this value is up to date! *)
         refresh ()

    (** Return the value contained by an EagerTotalOrder thunk, computing it if necessary. *)
    let force m =
      make_dependency_edge m ;
      m.value

    let viznode _ = failwith "viznode: not implemented"
end
include T

module Eviction = struct
  let flush () = ()
  let set_policy p = ()
  let set_limit n = ()
end

(** Functor to make constructors and updaters for EagerTotalOrder thunks of a specific type. *)
module MakeArt (Name:GrifolaType.NameType) (Data : DatType) :
GrifolaType.ArtType
       with type Name.t = Name.t
        and type Data.t = Data.t = struct

include T

module Name = Name
module Data = Data

(** Value contained by EagerTotalOrder thunks for a specific type. *)
type data = Data.t

(** EagerTotalOrder thunks for a specific type. *)
type t = Data.t thunk

(**/**) (* helper functions *)

let nop () = ()

let invalidator meta ts =
  (* help GC mark phase by cutting the object graph *)
  (* no need to call unmemo since the memo entry will be replaced when
     it sees start_timestamp is invalid; also, no need to replace
     {start,end}_timestamp with null since they are already cut by
     TotalOrder during invalidation *)
  Printf.printf "... marked invalid: %d\n%!" (TotalOrder.id ts);
  meta.unmemo <- nop;
  meta.evaluate <- nop;
  unqueue meta;
  WeakDyn.clear meta.dependents

let update m x =
  if not (Data.equal m.value x) then
    begin
      Printf.printf "... update: ** CHANGED: #%d (%d,%d) **\n%!" m.id (TotalOrder.id m.meta.start_timestamp) (TotalOrder.id m.meta.end_timestamp);
      m.value <- x;
      enqueue_dependents m.meta.dependents
    end

(** Create an EagerTotalOrder thunk from a constant value. *)
let const x =
  incr Statistics.Counts.create;
  let id = AdaptonTypes.Counter.next eager_id_counter in
  let m = {
    id=id;
    value=x;
    meta={
      mid=id;
      enqueued=false;
      evaluate=nop;
      unmemo=nop;
      start_timestamp=TotalOrder.null;
      end_timestamp=TotalOrder.null;
      dependents=WeakDyn.create 0;
    };
  } in
  m

(** Update an EagerTotalOrder thunk with a constant value. *)
let update_const m x =
  incr Statistics.Counts.update;
  if m.meta.start_timestamp != TotalOrder.null then
    begin
      (* no need to call unmemo since the memo entry will be replaced when it sees start_timestamp is invalid *)
      m.meta.unmemo <- nop;
      m.meta.evaluate <- nop;
      unqueue m.meta;
      TotalOrder.reset_invalidator m.meta.start_timestamp;
      TotalOrder.splice ~db:"update_const" ~inclusive:true m.meta.start_timestamp m.meta.end_timestamp;
      m.meta.start_timestamp <- TotalOrder.null;
      m.meta.end_timestamp <- TotalOrder.null
    end;
  update m x

let evaluate_meta meta f =
  incr Statistics.Counts.evaluate;
  eager_stack := meta::!eager_stack;
  let value = try
      Printf.printf "... BEGIN -- evaluate_meta: &%d @ (%d,%d):\n%!" meta.mid (TotalOrder.id meta.start_timestamp) (TotalOrder.id meta.end_timestamp) ;
      let res = f () in
      Printf.printf "... END -- evaluate_meta: &%d @ (%d,%d).\n%!" meta.mid (TotalOrder.id meta.start_timestamp) (TotalOrder.id meta.end_timestamp) ;
      res
    with exn ->
      eager_stack := List.tl !eager_stack;
      raise exn
  in
  eager_stack := List.tl !eager_stack;
  value

let make_evaluate m f =
  fun () -> update m (evaluate_meta m.meta f)

                   (*
(** Update an EagerTotalOrder thunk with a function that may depend on other EagerTotalOrder thunks. *)
let update_thunk m f =
  incr Statistics.Counts.update;
  if m.meta.start_timestamp != TotalOrder.null then
    begin
      m.meta.unmemo ();
      m.meta.unmemo <- nop;
      m.meta.evaluate <- nop;
      unqueue m.meta;
      TotalOrder.reset_invalidator m.meta.start_timestamp;
      TotalOrder.splice ~inclusive:true m.meta.start_timestamp m.meta.end_timestamp;
      m.meta.end_timestamp <- TotalOrder.null
    end;
  m.meta.start_timestamp <- add_timestamp ();
  let evaluate = make_evaluate m f in
  evaluate ();
  m.meta.end_timestamp <- add_timestamp ();
  TotalOrder.set_invalidator m.meta.start_timestamp (invalidator m.meta);
  m.meta.evaluate <- evaluate
                    *)

let cell nm v = const v (* TODO: Use the name; workaround: use mfn_nart interface instead. *)
let set = update_const

let make_and_eval_node f =
  incr Statistics.Counts.create;
  let id = AdaptonTypes.Counter.next eager_id_counter in
  let meta = {
    mid=id;
    enqueued=false;
    evaluate=nop;
    unmemo=nop;
    start_timestamp=add_timestamp ();
    end_timestamp=TotalOrder.null;
    dependents=WeakDyn.create 0;
  } in
  let v = evaluate_meta meta f in
  let m = { id=id; value=v; meta } in
  meta.end_timestamp <- add_timestamp ();
  TotalOrder.set_invalidator meta.start_timestamp (invalidator meta);
  meta.evaluate <- make_evaluate m f;
  m

let thunk nm f =
  (* TODO: Use the name; workaround: use mfn_nart interface instead. *)  
  make_and_eval_node f 

type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                  mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                  mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

let mk_mfn (type a)
      _
      (module Arg : DatType with type t = a)
      (user_function: Arg.t mfn -> Arg.t -> Data.t)
      : Arg.t mfn
    =
    let rec mfn =
      (* create memoizing constructors *)
      let module Memo = struct
        type data = Data.t
        type t = Data.t thunk

        (** Create memoizing constructor for an EagerTotalOrder thunk. *)
        module Binding = struct

          type key =
            | Arg of Arg.t
            | Name of Name.t

          type t = { key : key ;
                     arg : Arg.t ref ;
                     mutable value : Data.t thunk option
                   }

          let seed = Random.bits ()

          let hash a =
            match a.key with
            | Arg arg -> Arg.hash seed arg
            | Name name -> Name.hash seed name

          let equal a b = match a.key, b.key with
            | Arg a, Arg b -> Arg.equal a b
            | Name n, Name m -> Name.equal n m
            | _ -> false
        end
        module Table = Weak.Make (Binding)
        let table = Table.create 0
      end
      in

      (* memoizing constructor *)
      let rec memo arg =
        let binding = Memo.Table.merge Memo.table Memo.Binding.({ key=Arg arg; arg=ref arg; value=None }) in
        match binding.Memo.Binding.value with
        | Some m when TotalOrder.is_valid m.meta.start_timestamp
                      && TotalOrder.compare m.meta.start_timestamp !eager_now > 0
                      && TotalOrder.compare m.meta.end_timestamp !eager_finger < 0 ->
           incr Statistics.Counts.hit;
           TotalOrder.splice ~db:"memo" !eager_now m.meta.start_timestamp;
           eager_now := m.meta.end_timestamp;
           make_dependency_edge m;
           m

        | _ ->
           (* note that m.meta.unmemo indirectly holds a reference to binding (via unmemo's closure);
                            this prevents the GC from collecting binding from Memo.table until m itself is collected *)
           incr Statistics.Counts.create;
           incr Statistics.Counts.miss;
           (* let m = make_node (fun () -> user_function mfn (!(binding.Memo.Binding.arg)) ) in *)
           let m = make_and_eval_node (fun () -> let res = user_function mfn ( ! ( binding.Memo.Binding.arg ) ) in
                                                 Printf.printf "... Computed Result=`%s'.\n%!" (Data.string res) ;
                                                 res
                                      ) in
           m.meta.unmemo <- (fun () -> Memo.Table.remove Memo.table binding);
           binding.Memo.Binding.value <- Some m;
           make_dependency_edge m;
           m
      in

      (* memoizing constructor *)
      let rec memo_name name arg =
        let binding = Memo.Table.merge Memo.table Memo.Binding.({ key=Name name; arg=ref arg; value=None }) in
        match binding.Memo.Binding.value with
        | Some m when TotalOrder.is_valid m.meta.start_timestamp
                      && TotalOrder.compare m.meta.start_timestamp !eager_now > 0
                      && TotalOrder.compare m.meta.end_timestamp !eager_finger < 0 ->

           if Arg.equal arg (!(binding.Memo.Binding.arg)) then (
             Printf.printf "...   memo_name: match (Same arg): &%d -- (%d, %d) -- `%s`\n%!"
                           m.meta.mid
                           (TotalOrder.id m.meta.start_timestamp) (TotalOrder.id m.meta.end_timestamp)
                           (Arg.string arg);
             incr Statistics.Counts.hit;
             TotalOrder.splice ~db:"memo_name: Same arg" !eager_now m.meta.start_timestamp;
             Printf.printf "... --  BEGIN -- refresh_until: match &%d (Same arg)\n%!" m.meta.mid ;
             refresh_until (Some m.meta.end_timestamp);
             eager_now := m.meta.end_timestamp;
             Printf.printf "... --  END   -- refresh_until: match &%d (Same arg)\n%!" m.meta.mid ;
             make_dependency_edge m;
             m
           )
           else (
             Printf.printf "...   memo_name: match (Diff arg): (%d, %d) -- `%s' (old) != `%s' (new)\n%!"
                           (TotalOrder.id m.meta.start_timestamp) (TotalOrder.id m.meta.end_timestamp)
                           (Arg.string !(binding.Memo.Binding.arg)) (Arg.string arg) ;
             Printf.printf "... ** BEGIN -- memo_name: match (Diff arg)\n%!" ;
             TotalOrder.splice ~db:"memo_name: Diff arg: #1" !eager_now m.meta.start_timestamp;
             eager_now := m.meta.start_timestamp;
             let old_finger = !eager_finger in
             assert( TotalOrder.compare m.meta.end_timestamp old_finger < 0 );
             eager_finger := m.meta.end_timestamp;
             binding.Memo.Binding.arg := arg ;
             m.meta.evaluate ();
             assert( TotalOrder.compare m.meta.start_timestamp !eager_now <= 0 );
             assert( TotalOrder.compare !eager_now   m.meta.end_timestamp <  0 );
             Printf.printf "... Start: splice (%d, %d)\n%!" (TotalOrder.id !eager_now) (TotalOrder.id m.meta.end_timestamp);
             TotalOrder.splice ~db:"memo_name: Diff arg: #2" !eager_now m.meta.end_timestamp;
             Printf.printf "... End: splice (%d, %d)\n%!" (TotalOrder.id !eager_now) (TotalOrder.id m.meta.end_timestamp);
             eager_finger := old_finger;
             Printf.printf "... ** END   -- memo_name: match (Diff arg)\n%!" ;
             assert ( TotalOrder.is_valid m.meta.start_timestamp ) ;
             assert ( TotalOrder.is_valid m.meta.end_timestamp ) ;
             eager_now := m.meta.end_timestamp;
             make_dependency_edge m;
             m
           )

        | _ ->
           (* note that m.meta.unmemo indirectly holds a reference to binding (via unmemo's closure);
                            this prevents the GC from collecting binding from Memo.table until m itself is collected *)
           incr Statistics.Counts.create;
           incr Statistics.Counts.miss;
           binding.Memo.Binding.arg := arg ; (* BUG FIX *)
           let m = make_and_eval_node (fun () -> let res = user_function mfn ( ! ( binding.Memo.Binding.arg ) ) in
                                                 Printf.printf "... Computed Result=`%s'.\n%!" (Data.string res) ;
                                                 res
                                      ) in
           m.meta.unmemo <- (fun () -> Memo.Table.remove Memo.table binding);
           binding.Memo.Binding.value <- Some m;
           make_dependency_edge m;
           m
      in

      (* incr Statistics.Counts.evaluate;  *)
      {
        mfn_data = (fun arg -> user_function mfn arg) ;
        mfn_art  = (fun arg -> memo arg) ;
        mfn_nart = (fun name arg -> memo_name name arg) ;
      }
    in mfn


end

(*
(** Tweak GC for this module. *)
let tweak_gc () =
  let open Gc in
  let control = get () in
  set { control with
        minor_heap_size = max control.minor_heap_size (2 * 1024 * 1024);
        major_heap_increment = max control.minor_heap_size (4 * 1024 * 1024);
      }

 *)
