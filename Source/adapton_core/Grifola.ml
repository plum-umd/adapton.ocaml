(** Grifola: Clean rewrite of the original Adapton module.

    (Named after this: http://en.wikipedia.org/wiki/Grifola_frondosa).

    Changelog (compared to Yit's original version, circa PLDI 2014):
    =========
    - New representation of unevaluated thunks
     (Cache-on-force, not cache-on-create, as before)

    - Supports non-generative thunks, and *full* eviction of
    (non-generative) thunks from cache (node reverts to a
    re-evaluation state, which requires no globally-registered
    representation).

    - Grifola uses a new internal representation that supports
    ref-counting to address the consistency issue related to weak refs
    and GC.

    - Grifola implements the old API (see module type AType).

    - Grifola implements the new API (see module type GrifolaType).
**)

open Primitives

module Statistics = AdaptonStatistics
module Key    = Key    (* re-export *)
module Symbol = Symbol (* re-export *)
module Viz    = Viz

module type AParamsType = sig
  val eviction_policy     : [`None | `Fifo of int | `Lru of int ]
  val eviction_time       : [`On_create | `On_flush ] (* TODO: Make deprecated: On_create is too tricky; always use On_flush. *)
  val ref_count           : bool
  val dirty_exactly       : bool
  val check_receipt       : bool
  val sanitize_pointers   : bool
  val disable_names       : bool
  val generative_ids      : bool
  val disable_mfns        : bool
  val debug_assert        : bool
end

module Make (Params:AParamsType) = struct
  let seeded_hash = Hashtbl.seeded_hash

  module type SuspType = sig
    module Mfn : MfnType
    type t (* Abstract suspension: Memoized function paired with an argument. *)
    val get_key      : t -> [`Name of Mfn.Name.t|`Arg of Mfn.Arg.t|`ArgGen of Mfn.Arg.t * int option]
    val get_key_hash : t -> int
    val get_arg      : t -> Mfn.Arg.t
    val set_arg      : t -> Mfn.Arg.t -> unit (* Error if thunk is not nominal. *)
    val set_id       : t -> int -> unit (* Error if thunk is not generative. *)
    val get_res      : t -> Mfn.res
  end

  module type SuspValType = sig
    module Susp : SuspType
    val susp : Susp.t
  end

  type dcg_state    = Consistent | Maybe_inconsistent
  type receipt      = { check  : 'cxt . (bool * dcg_state -> 'cxt) -> 'cxt }
  type 'a w_receipt = ( 'a * receipt * dcg_state )
  type 'a evaluate  = unit -> 'a w_receipt
  type 'a repair    = { repair : 'cxt . ('a w_receipt -> 'cxt) -> 'cxt }
    
  let dcg_state_meet s1 s2 = match s1, s2 with
    | Consistent, Consistent -> Consistent
    | _,          _          -> Maybe_inconsistent

  (* The set of "meta nodes" constitutes one layer of the DCG.  They
     each carry a unique ID.  Their pointers represent the backward
     direction of dependencies in the DCG in a way that suffices to
     perform backward traversals of the dependency graph, from (meta)
     nodes that are created or forced, to the (meta) nodes that create
     or force them.  We use these traversals to communicate the
     presence of input changes to nodes that may be affected by
     them. To do so, we mark these dependency edges as "dirty".  XXX:
     In some cases, we also mark affected nodes as "filthy". *)
  module rec Meta_node : sig
    type meta_node = {
      id : int ;
      mutators  : Mutators.t ; (* Incoming creation edges *)
      dependents : Dependents.t; (* Incoming force edges *)
      mutable meta_state : meta_state ;
      viznode : Viz.node ;
    }
    and meta_state = Ok | Filthy
    and dependency = {
      mutable undo : bool -> unit ;
      mutable flag : flag;
      mutable receipt : receipt;
      dependent : meta_node ;
      dependee  : meta_node Log.ifdef;
    }
    and mut_edge = {
      mutable mut_undo : bool -> unit ;
      mutable mut_flag : flag ;
      mut_source : meta_node ;
      mut_target : meta_node Log.ifdef ;
    }
    and flag =
    | Clean
    | Dirty | Dirty_to_clean
    | Obsolete_edge
  end = Meta_node
  and Dependents : WeakSet.S with type data = Meta_node.dependency = WeakSet.Make (
    struct
      type t = Meta_node.dependency
      let hash d = Hashtbl.hash d.Meta_node.dependent.Meta_node.id
      let equal d d' = d == d'
    end)
  and Mutators : WeakSet.S with type data = Meta_node.mut_edge = WeakSet.Make (
    struct
      type t = Meta_node.mut_edge
      let hash e = Hashtbl.hash e.Meta_node.mut_source.Meta_node.id
      let equal e e' = e == e'
    end)
  include Meta_node

  (* A suspension node (aka a "Thunk node") represents a (lazy)
     function application that, when forced, computes a value that is
     subject to change over time.  Each suspension node in the DCG
     stores a computed value, a way to check whether this value has
     changed, and a way to repair this value when it is affected by
     other changes.

     Additionally, for garbage collection purposes, each node stores
     pointers to the representation of its (outgoing) dependencies,
     i.e., the (meta) nodes that it creates or forces. In the meta
     layer, pointers to these edges are weak. Each node's strong
     pointers to these edges keep them live for as long as the node is
     live. *)
  module Susp_node = struct
    type 'a susp = (module SuspValType with type Susp.Mfn.res = 'a)

    let susp_equal (type a) (s1: a susp) (s2: a susp) =
      let module S1 : SuspValType with type Susp.Mfn.res = a = (val s1) in
      let module S2 : SuspValType with type Susp.Mfn.res = a = (val s2) in
      if S1.Susp.Mfn.id <> S2.Susp.Mfn.id then false
      else
        let s1_key = S1.Susp.get_key S1.susp in
        let s2_key = Obj.magic (S2.Susp.get_key S2.susp) in
        match s1_key, s2_key with
        | `Name n1, `Name n2 -> n1 == n2 || (S1.Susp.Mfn.Name.equal n1 n2)
        | `Arg a1,  `Arg a2  -> a1 == a2 || (S1.Susp.Mfn.Arg.equal a1 a2)
        | `ArgGen(a1,id1), `ArgGen(a2,id2) -> 
          ( match id1, id2 with
          | Some id1, Some id2 -> id1 = id2
          | _ -> a1 == a2 || (S1.Susp.Mfn.Arg.equal a1 a2)
          )          
        | _, _ -> false

    let susp_hash (type a) seed (s: a susp) =
      let module S : SuspValType with type Susp.Mfn.res = a = (val s) in
      seeded_hash (seeded_hash seed S.Susp.Mfn.id) (S.Susp.get_key_hash S.susp)

    let susp_string (type a) (s: a susp) =
      let module S : SuspValType with type Susp.Mfn.res = a = (val s) in
      Printf.sprintf "Susp(%s$%d, %s)" (S.Susp.Mfn.name()) S.Susp.Mfn.id
        (match S.Susp.get_key S.susp with
        | `Name n -> "Name "^(S.Susp.Mfn.Name.string n)
        | `Arg a  -> "Arg "^(S.Susp.Mfn.Arg.string a)
        | `ArgGen(a,id) -> "ArgGen "^(S.Susp.Mfn.Arg.string a)
          ^"#"^(match id with None -> "?" | Some id -> string_of_int id)
        )

    type 'a susp_state = { (* susp_state does not point back at its node. *)
      susp_evaluate : 'a evaluate ;
      susp_receipt  : receipt ;
      susp_repair   : 'a repair ;
      susp_value    : unit -> 'a ; (* Invariant: Freshly sanitized. *)
      (* For garbage collection:
         Each node stores its outgoing edges, keeping them live: *)
      susp_forces   : Meta_node.dependency list ;
      susp_creates  : Meta_node.mut_edge list ;
    }
    and 'a susp_node = {
      susp_meta          : meta_node ;
      mutable susp_state : 'a susp_state ;
      mutable susp_refc  : int ;
      susp_undo          : unit -> unit ; (* remove node from memo table *)
    }

    let susp_node_string sn =
      (Printf.sprintf "%d" sn.susp_meta.id)

    type prenode = {
      prenode_meta : meta_node ;
      prenode_undo : unit -> unit ;
      mutable prenode_refc : int ;
    }

    type 'a susp_ptr = {
      susp_susp         : 'a mfn_res susp ;
      mutable susp_node : 'a shared_susp_node ;
    }

    and 'a mfn_res = 'a susp_ptr -> meta_node * 'a mfn_callback

    and 'a mfn_callback = 
      (* A Mfn Callback is closed over a susp_ptr.
         The callback updates the susp_ptr by modifying its susp_node field.
         It returns a value of type ['a w_receipt], which can be viewed as the "DCG Monad".
      *) 
      unit -> 'a w_receipt

    and 'a shared_susp_node = ('a susp_node_option) ref

    and 'a vacancy = { vac_susp : 'a susp_ptr ;
                       mutable vac_refc : int }

    and 'a susp_node_option =
    | Empty
    | Prenode of prenode
    | Node    of 'a susp_node
    | Vacant  of 'a vacancy

    (* Node states are cyclic: Through closures, a susp_node's state points back at the susp_node.
       Recipe for creating node representation:
       - (1) Make the (shared) meta_node.
       - (2) Make shared ref holding a Prenode.
       - (3) Make the first state of type susp_state by calling susp_evaluate on (1) and (2).
       (The accesses from the shared_susp_node are now suspended in closures of susp_state).
       - Make the node sn, of type susp_node, using the first state.
       - Back-patch the ref with the node sn, setting it to Node sn.
       (This concludes the setup phase, which need not occur again, unless the node is evicted).
       - The susp_evaluate function (a susp_state field) backpatches the state of sn.
       
       Empty state: 
       The node never transitions back to being Empty.       

       TODO: Explain these concepts:
       Node Eviction:
       Vacant state.
    *)

    let susp_ptr_equal (sp1: 'a susp_ptr) (sp2: 'a susp_ptr) =
      match !(sp1.susp_node), !(sp2.susp_node) with
      | Node n1, Node n2 ->
        incr Statistics.Count_eq.post_force ;
        n1 == n2
      | _ ->
        incr Statistics.Count_eq.one_pre_force ;
        susp_equal sp1.susp_susp sp2.susp_susp

    let susp_ptr_hash seed (sp: 'a susp_ptr) =
      susp_hash seed sp.susp_susp

    let susp_ptr_copy (sp: 'a susp_ptr) =
      { susp_node = sp.susp_node ;
        susp_susp = sp.susp_susp }

    let shared_susp_node_meta (ssn:'a shared_susp_node) : meta_node =
      match !ssn with
      | Node sn -> sn.susp_meta
      | Prenode pn -> pn.prenode_meta
      | Vacant _ -> failwith "shared_susp_node_meta: Expected Node _, not Vacant _"
      | Empty -> failwith "shared_susp_node_meta: Expected Node _, not Empty"

    let meta_of_shared_susp_node (ssn:'a shared_susp_node) : meta_node = 
      shared_susp_node_meta ssn

    let refc_of_shared_susp_node (ssn:'a shared_susp_node) : int = 
      match !ssn with
      | Empty -> 0
      | Vacant vac -> vac.vac_refc
      | Prenode pn -> pn.prenode_refc
      | Node n     -> n.susp_refc

    let incr_refc_of_shared_susp_node (ssn:'a shared_susp_node) : unit = 
      match !ssn with
      | Empty    -> failwith "incr_refc_of_shared_susp_node: Expected Prenode _ | Node _, not Empty"
      | Vacant vac -> ( vac.vac_refc    <- vac.vac_refc + 1 )
      | Prenode pn -> ( pn.prenode_refc <- pn.prenode_refc + 1 )
      | Node n     -> ( n.susp_refc     <- n.susp_refc + 1 )
        
    let undo_of_shared_susp_node (ssn:'a shared_susp_node) : (unit->unit) = 
      match !ssn with
      | Empty      -> failwith "undo_of_shared_susp_node: Expected Prenode _ | Node _, not Empty"
      | Vacant vac -> failwith "undo_of_shared_susp_node: Expected Prenode _ | Node _, not Vacant"
      | Prenode pn -> pn.prenode_undo
      | Node n     -> n.susp_undo

  end (* Susp_node *)
  include Susp_node

  module Mut_node = struct
    type 'a mut_state = {
      mut_value   : 'a ;
      mut_receipt : receipt ;
    }
    type 'a mut_node = {
      mut_meta          : meta_node ;
      mutable mut_state : 'a mut_state ;
    }
  end (* Mut_node *)
  include Mut_node

  module Node = struct
    type 'a node =
    | Susp_ptr of 'a susp_ptr
    | Mut_node of 'a mut_node

    let node_equal n1 n2 =
      match n1, n2 with
      | Mut_node _, _            -> incr Statistics.Count_eq.mut_cell ; n1 == n2
      | _, Mut_node _            -> incr Statistics.Count_eq.mut_cell ; n1 == n2
      | Susp_ptr s1, Susp_ptr s2 -> Susp_node.susp_ptr_equal s1 s2

    let node_hash seed n =
      incr Statistics.Count_eq.hash ;
      match n with
      | Mut_node n -> seeded_hash seed n.mut_meta.id
      | Susp_ptr p -> susp_ptr_hash seed p

    let node_string n =
      match n with
      | Mut_node mn -> (Printf.sprintf "(Mut %d)" mn.mut_meta.id)
      | Susp_ptr sp ->
        Printf.sprintf "(Sp %s %s)"
          (match !(sp.susp_node) with
          | Empty -> "Empty"
          | Prenode pn -> (Printf.sprintf "(Meta %d)" pn.prenode_meta.id)
          | Vacant x -> (Printf.sprintf "(Vacant refc=%d)" x.vac_refc)
          | Node sn -> susp_node_string sn
          )
          (susp_string sp.susp_susp)

    let node_sanitize n =
      if not Params.sanitize_pointers then
        n
      else
        match n with
        | Mut_node mn -> n (* Mut nodes always use physical equality. *)
        | Susp_ptr p  ->  (* make shallow copy of n; sanitized ==> no finalizers *)
          let n_copy = Susp_ptr p in
          assert (not (n == n_copy)) ; n_copy

    (** Return the id of an Adapton thunk. *)
    let node_id node =
      incr Statistics.Count_eq.id ;
      match node with
      | Mut_node m -> Some m.mut_meta.id
      | Susp_ptr p -> match !(p.susp_node) with              
        | Node n     -> Some n.susp_meta.id
        | Prenode pn -> Some pn.prenode_meta.id
        | Vacant _   -> None
        | Empty      -> None

    let node_meta node =
      match node with
      | Mut_node m -> m.mut_meta
      | Susp_ptr sp -> shared_susp_node_meta sp.susp_node
  end (* Node *)
  include Node

  (* Global memo table registry. *)
  (* Provides a generic interface for interacting with memotables in the global registry. *)
  (* Unlike the programming interface, this interface does not produce node handles. *)
  module Memotables : MemotablesType = struct
    let registry : ((module MemotableType) list) ref = ref []

    let register memotable =
      if (IFDEF ADAPTON_LOG THEN true ELSE false ENDIF) (* XXX *) then (
        registry := memotable :: (!registry) ;
      )

    let print_stats out =
      List.iter begin fun (module Mt : MemotableType) ->
        Printf.fprintf out "Memotable %d: {name:%-32s, length:%d, numadd:%d, numrem:%d}\n%!"
          Mt.Mfn.id (Mt.Mfn.name()) (Mt.length ()) (Mt.numadd()) (Mt.numrem())
      end (!registry)

    let fold a f =
      List.fold_right begin fun (module Mt : MemotableType) a ->
        f a (module Mt : MemotableType)
      end (!registry) a

  end (* Memotables *)

  (* Counters:
     [global_seqno] is used for XXX, and visualization.
     [next_id] is used to identify [meta_node]s as they are created.
  *)
  let global_seqno = ref 0
  let _ = Viz.global_seqno_hook := (fun () -> !global_seqno)
  let next_id = AdaptonTypes.Counter.make 1 (* 0 is for root node. *)

  type stack_frame = {
    stkf_edge_src          : meta_node ;
    mutable stkf_obs_edges : dependency list ;
    mutable stkf_mut_edges : mut_edge list ;
  }
  type force_stack = ( stack_frame list ) ref
  let force_stack : force_stack = ref []

  (* An "eviction policy" decides when, if ever, to purge memo table
     entries by calling susp_ptr_evict, defined below. *)
  module type Eviction_policy = sig
    val string   : unit -> string
    val create   : 'a susp_ptr -> meta_node -> 'a shared_susp_node -> unit (* notifies policy of a new node; allows policies to purge old nodes. *)
    val share    : 'a susp_ptr -> 'a susp_node -> unit (* notifies policy of subsequent creations, after the initial one. *)
    val force    : 'a susp_ptr -> 'a susp_node -> unit (* notifies policy of a node being forced. *)
    val refc_incr : 'a susp_ptr -> (*'a susp_node ->*) unit (* notifies policy reference count increment. *)
    val refc_decr : 'a susp_ptr -> (*'a susp_node ->*) unit (* notifies policy reference count decrement. *)
    val flush    : unit -> unit    (* notifies the policy that the user called for a flush. *)
  end

  module No_eviction_policy : Eviction_policy = struct
    let string () = "No_eviction_policy"
    let create sp meta ssn = ()
    let share sp sn = ()
    let force sp sn = ()
    let refc_incr sp (*sn*) = ()
    let refc_decr sp (*sn*) = ()
    let flush () = ()
  end

  let eviction_policy = ref (module No_eviction_policy : Eviction_policy)

  module Undo_buff = Set.Make(
    struct
      type t = meta_node * ((* closure pointing at susp_ptr *) bool -> unit)
      let compare a b = Pervasives.compare ((fst a).id) ((fst b).id)
    end)
  let undo_buff : Undo_buff.t ref = ref Undo_buff.empty

  let string_of_flag = function
    | Dirty_to_clean -> "Dirty_to_clean"
    | Dirty -> "Dirty"
    | Clean -> "Clean"
    | Obsolete_edge -> "Obsolete_edge"

  let pv_of_flag = function
    | Dirty -> `Dirty
    | Clean -> `Clean
    | Dirty_to_clean -> `Dirty_to_clean
    | Obsolete_edge -> `Obsolete

  let nil_undo _ = () (* empty closure; helps speed up GC after DCG undoing. *)

  let current_meta_node () : meta_node option =
    match !force_stack with
    | []      -> None
    | stkf::_ -> Some (stkf.stkf_edge_src)

  let meta_is_root : meta_node -> bool =
    fun m -> ( m.id = 0 )

  let force_src : unit -> meta_node =  (* For logs / debugging. *)
    let root_meta = { id = 0;
                      mutators = Mutators.create 0 ;
                      dependents = Dependents.create 0 ;
                      meta_state = Ok ;
                      viznode = Viz.make_node 0 None None ; }
    in
    (assert (!next_id = 1));
    fun () -> match current_meta_node () with
    | Some m -> m
    | None -> root_meta

  let make_meta_with key sym =
    let id = AdaptonTypes.Counter.next next_id in
    let meta = {
      mutators=Mutators.create 0;
      id=id;
      dependents=Dependents.create 0;
      meta_state=Ok;
      viznode=Viz.make_node id key sym;
    } in
    meta

  let make_meta () = make_meta_with None None

  let dirty (meta:meta_node) =
    Viz.tick (Viz.Dirty_pre meta.viznode);
    let rec dirty : Dependents.t list -> unit = function
      | d::ds -> dirty
        begin Dependents.fold begin fun d ds ->
          if d.flag == Clean (* BUGFIX: d.flag <> Dirty *) then (
            IFDEF ADAPTON_LOG THEN (
              Viz.tick (Viz.Dirty_edge(d.dependent.viznode, d.dependee.viznode))
            ) END ;
            incr Statistics.Counts.dirty;
            d.flag <- Dirty;
            d.dependent.dependents::ds
          ) else (* either Dirty or Obsolete *)
            ds
        end d ds
        end
      | [] ->
        ()
    in
    dirty [ meta.dependents ] ;
    Viz.tick (Viz.Dirty_post meta.viznode)

  let mark_filthy (meta:meta_node) =
    Viz.tick (Viz.Mark_filthy(meta.viznode,true));
    match meta.meta_state with
    | Filthy -> ()
    | Ok -> ( meta.meta_state <- Filthy ;
              dirty meta )

  let rec susp_node_undo_edges (sn:'a susp_node) (undo_now:bool) =
    let creates = sn.susp_state.susp_creates in
    let forces = sn.susp_state.susp_forces in
    List.iter (fun d -> d.flag <- Obsolete_edge ; d.undo undo_now ; d.undo <- nil_undo) forces ;
    List.iter (fun d -> d.mut_flag <- Obsolete_edge ; d.mut_undo undo_now ; d.mut_undo <- nil_undo) creates

  let susp_node_decr_refc =
    let susp_node_undo (sp:'a susp_ptr) (sn:'a susp_node) (undo_now:bool) =
      assert Params.ref_count ;
      let delay meta_node undo_f =
        if not (Undo_buff.mem (meta_node, undo_f) (!undo_buff)) then
          undo_buff := (Undo_buff.add (meta_node, undo_f) (!undo_buff))
      in
      assert Params.ref_count ;
      (if undo_now then (fun f -> f true) else (fun f -> delay sn.susp_meta f)) begin fun undo_now ->
        assert ( sn.susp_refc >= 0 ) ;
        let is_node n = match n with 
          | Prenode _ -> false 
          | Vacant _  -> false 
          | Empty     -> false 
          | Node _    -> true
        in
        if sn.susp_refc = 0 && (is_node !(sp.susp_node)) then begin
          (* No more references; "Destroy" the node (it is Obsolete). *)
          Viz.tick (Viz.Undo_pre sn.susp_meta.viznode) ;
          incr Statistics.Counts.destroy;
          incr Statistics.Counts.destroy_refc;
          sn.susp_undo () ;
          susp_node_undo_edges sn true ;
          sp.susp_node := Vacant {vac_refc=sn.susp_refc;vac_susp=sp} ; (assert (sn.susp_refc = 0)) ;
          Viz.tick (Viz.Undo_post sn.susp_meta.viznode) ;
        end
        else begin (* Node is either already Obsolete or is still alive. *)
          if sn.susp_refc <> 0 then
            Viz.tick (Viz.Undo_skip (sn.susp_meta.viznode, sn.susp_refc))
        end
      end
    in
    fun (sp:'a susp_ptr) (undo_now:bool) ->
      assert Params.ref_count ;
      match !(sp.susp_node) with
      | Empty -> failwith "impossible: node cannot be Empty"
        
      | Prenode pn ->
        if not ( pn.prenode_refc > 0 ) then
          failwith "susp_node_decr_refc/Prenode: expected positive reference count"
        ; 
        Viz.tick (Viz.Refc_decr (pn.prenode_meta.viznode, pn.prenode_refc)) ;
        pn.prenode_refc <- pn.prenode_refc - 1 ;
        
      | Vacant vac ->
        if not ( vac.vac_refc > 0 ) then
          failwith 
            (Printf.sprintf "susp_node_decr_refc/Vacant: %s: expected positive reference count"
               (let module E = (val !eviction_policy) in E.string ()))
        ; 
        (* XXX: Find some way to tick this, even though there is no viznode. *)
        vac.vac_refc <- vac.vac_refc - 1 ;
        
      | Node sn ->
        assert ( sn.susp_refc > 0 ) ;
        sn.susp_refc <- sn.susp_refc - 1 ;
        let module E = (val !eviction_policy) in E.refc_decr sp ;
        Viz.tick (Viz.Refc_decr (sn.susp_meta.viznode, sn.susp_refc)) ;
        if sn.susp_refc = 0 then
          susp_node_undo sp sn undo_now

  let susp_ptr_evict (sp:'a susp_ptr) =
    (* Immediately reverts an evaluated node into a pre-evaluated state. *)
    (* By-passes the undo buffer ordinarly used by reference counting. *)
    incr Statistics.Counts.evict ;
    match !(sp.susp_node) with
    | Vacant n -> () (* Nothing to do. Already in post-evicted state. *)
      
    | Empty -> 
      sp.susp_node := (Vacant {vac_refc=0; vac_susp=sp})
        
    | Prenode pn -> 
      sp.susp_node := (Vacant {vac_refc=pn.prenode_refc; vac_susp=sp})
        
    | Node sn -> let meta = sn.susp_meta in
                 incr Statistics.Counts.destroy;
                 incr Statistics.Counts.destroy_evict;
                 Viz.tick (Viz.Revert(meta.viznode));
                 dirty meta ; (* mark dependent edges as dirty. *)
                 sn.susp_undo () ; (* e.g.: remove from memo table. *)
                 susp_node_undo_edges sn false ; (* e.g.: decrement reference counts. *)
                 (* vacate node reference..but maintain reference
                    count defined by current context (which is
                    unaffected): *)
                 sp.susp_node := (Vacant {vac_refc=sn.susp_refc; vac_susp=sp})

  let node_evict (n:'a node) =
    match n with
    | Mut_node _ -> failwith "cannot evict Mut_node"
    | Susp_ptr sp -> susp_ptr_evict sp

  let flush () =
    if Params.ref_count then begin
      let undos = !undo_buff in
      undo_buff := Undo_buff.empty ;
      Undo_buff.iter (fun (_,undo_f) -> undo_f true (* XXX make this a polyvariant type *)) undos ;
    end ;
    let module E = (val !eviction_policy) in E.flush ()

  let susp_ptr_force (type a) (sp: a susp_ptr) =
    let module S = (val sp.susp_susp: SuspValType
                    with type Susp.Mfn.res = a mfn_res) in
    let meta_node, cont = S.Susp.get_res S.susp sp in
    let force_src = force_src () in
    Viz.tick (Viz.Force_pre (force_src.viznode, meta_node.viznode));
    let value, receipt, dcg_state = cont () in
    let pre_vacant_post_node = match !(sp.susp_node) with
      | Empty -> failwith "impossible: forced node cannot be Empty"
      | Prenode _ -> failwith "impossible: forced node cannot be Prenode _"
      | Vacant _ -> false (* false => Vacant *)
      | Node sn -> true (* true => Node *)
    in
    assert pre_vacant_post_node ;
    (meta_node, value, receipt, dcg_state, pre_vacant_post_node)

  let node_force (type a) (node: a node) =
    let force_src = force_src () in
    let (meta, value, receipt, dcg_state, (pre_vacant_post_filled:bool)) =
      match node with
      | Mut_node m ->
        Viz.tick (Viz.Force_pre (force_src.viznode, m.mut_meta.viznode));
        let s = m.mut_state in
        (m.mut_meta, s.mut_value, s.mut_receipt, Consistent, false)

      | Susp_ptr sp ->
        match !(sp.susp_node) with
        | Empty -> susp_ptr_force sp
        | Vacant x -> susp_ptr_force sp
        | Prenode pn -> susp_ptr_force sp
        | Node n ->
          let module E = (val !eviction_policy) in E.force sp n ;
          Viz.tick (Viz.Force_pre (force_src.viznode, n.susp_meta.viznode));
          let s = n.susp_state in
          let value, receipt, dcg_state = s.susp_repair.repair (fun x -> x) in
          (n.susp_meta, value, receipt, dcg_state, false)
    in
    begin match !force_stack with
    | [] -> begin (* Created by root: Consider attaching a finaliser for RC logic: *)
      if Params.ref_count && pre_vacant_post_filled then
        match node with
        | Mut_node _ -> () (* Nothing to do. *)
        | Susp_ptr p -> match !(p.susp_node) with
          | Empty     -> failwith "impossible: Cannot be Empty. Node was just forced."
          | Prenode _ -> failwith "impossible: Cannot be Prenode _. Node was just forced."
          | Vacant _  -> failwith "impossible: Cannot be Vacant _. Node was just forced."
          | Node n ->
            n.susp_refc <- n.susp_refc + 1 ;
            let module E = (val !eviction_policy) in E.refc_incr p ;
            Viz.tick (Viz.Refc_incr (n.susp_meta.viznode, n.susp_refc)) ;
            (* No physical root node: Add GC finalizer to susp pointer: *)
            Viz.tick (Viz.Add_finaliser(n.susp_meta.viznode)) ;
            IFDEF ADAPTON_LOG THEN (
              let finalisers = n.susp_meta.viznode.Viz.finalizers () in
              n.susp_meta.viznode.Viz.finalizers <- (fun () -> finalisers+1);
            ) END ;
            Gc.finalise
              (fun node ->
                match node with
                | Mut_node _ -> failwith "impossible: Cannot be Mut_node _"
                | Susp_ptr sp ->
                  IFDEF ADAPTON_LOG THEN (
                    match !(sp.susp_node) with Node sn ->
                      Viz.tick (Viz.Run_finaliser(sn.susp_meta.viznode)) ;
                    | _ -> ()
                  ) END ;
                  susp_node_decr_refc sp false
              ) node ;
    end

    | stkf :: _ ->
      let dependency' =
        { flag=(match dcg_state with
        | Consistent         -> Clean 
        | Maybe_inconsistent -> Dirty);
          receipt;
          dependent=stkf.stkf_edge_src;
          undo=nil_undo;
          dependee=(IFDEF ADAPTON_LOG THEN meta END); }
      in
      let dependency = Dependents.merge meta.dependents dependency' in
      assert ( dependency == dependency' ) ;
      stkf.stkf_obs_edges <- dependency::stkf.stkf_obs_edges ;
      if Params.ref_count then
            (* Created by an interior node: Use undo on edge for RC logic: *)
        match node with
        | Mut_node _ -> ()
        | Susp_ptr p -> match !(p.susp_node) with
          | Empty -> failwith "impossible: Cannot be Empty. Already case-analyzed this."
          | Prenode _ -> failwith "impossible: Cannot be Prenode _. Already case-analyzed this."
          | Vacant vac ->
            vac.vac_refc <- vac.vac_refc + 1 ;
            (* TODO: Somehow do a Viz.tick here. No viznode available. *)
            dependency.undo<-(fun undo_now -> susp_node_decr_refc p undo_now)

          | Node n ->
            n.susp_refc <- n.susp_refc + 1 ;
            let module E = (val !eviction_policy) in E.refc_incr p ;
            Viz.tick (Viz.Refc_incr (n.susp_meta.viznode, n.susp_refc)) ;
                      (* RC logic for when edge is removed: *)
            dependency.undo<-(fun undo_now -> susp_node_decr_refc p undo_now)
    end;
    Viz.tick (Viz.Force_post(force_src.viznode,meta.viznode));
    (* Invariant: Value is sanitized. Any added finalizers are not
       associated with pointers interior to the DCG: *)
    value

  module MakeArt
    (Name:GrifolaType.NameType) 
    (Data:DatType) 
    : GrifolaType.ArtType 
    with type Name.t = Name.t
    and  type Data.t = Data.t 
    and  type t = Data.t node   = 
  struct
    module Name = Name
    module Data = Data

    type t = Data.t node

    (* Namespaces: Memo-table-based recursion ( nominal & classic ). *)
    type 'arg mfn = { mfn_data : 'arg -> Data.t ;      (* Pure recursion. *)
                      mfn_art  : 'arg -> t ;           (* Create a memoized articulation, classically. *)
                      mfn_nart : Name.t -> 'arg -> t ; (* Create a memoized articulation, nominally. *) }

    let string   = node_string
    let hash     = node_hash
    let equal    = node_equal
    let force    = node_force
    let sanitize = node_sanitize

    let mut_node_check (mn:Data.t mut_node) (x:Data.t) k =
      if Params.check_receipt && (not Params.disable_mfns) then
        k ((Data.equal mn.mut_state.mut_value x), Consistent)
      else
        k (false, Consistent)

    (** Create a mutable ref cell. *)
    let cell (nm:Name.t) (x:Data.t) =
      (* XXX: Use name. *)
      incr Statistics.Counts.create;
      let rec check : 'a . (bool * dcg_state -> 'a) -> 'a = 
        fun k -> mut_node_check n x k
      and n = {
        mut_meta=make_meta();
        mut_state={mut_value=x;
                   mut_receipt={check}}} in
      IFDEF ADAPTON_LOG THEN(
        let viznode = n.mut_meta.viznode in
        viznode.Viz.refc <- (fun () -> None);
        viznode.Viz.arg_string <- (fun _ -> "");
        viznode.Viz.val_string <- (fun _ -> Data.string n.mut_state.mut_value);
      ) END ;
      (Mut_node n)

    (** Imperatively update a mutable ref cell. *)
    let set (cell:Data.t node) (x:Data.t) =      
      incr Statistics.Counts.update;
      incr global_seqno ;
      assert ( !force_stack = [] ) ; (* for now, set only used by outer layer code. *)
      match cell with
      | Susp_ptr _ -> failwith "set: expected Mut_node, not Susp_ptr"
      | Mut_node n ->
        if not (Data.equal n.mut_state.mut_value x) then begin
          dirty n.mut_meta ;
          let check k = mut_node_check n x k in
          n.mut_state <- {mut_value=x; mut_receipt={check}}
        end

    let dcg_state_of_forces forces = 
      List.fold_right 
        (fun d dcg_state -> match d.flag with 
        | Clean -> dcg_state
        | Dirty -> Maybe_inconsistent
        | Obsolete_edge -> Maybe_inconsistent (* Source node was evicted. *)
        | Dirty_to_clean -> failwith "expected Clean | Dirty, not Dirty_to_clean"
        ) forces Consistent

    let dcg_state_of_mutations mutations = 
      List.fold_right 
        (fun d dcg_state -> match d.mut_flag with 
        | Clean -> dcg_state
        | Dirty -> Maybe_inconsistent
        | Obsolete_edge -> Maybe_inconsistent (* Source node was evicted. *)
        | Dirty_to_clean -> failwith "expected Clean | Dirty, not Dirty_to_clean"
        ) mutations Consistent

    (* Logic for both initial evaluation and re-evaluation of suspension nodes. *)
    let rec susp_evaluate
        (shared_susp_node:Data.t shared_susp_node)
        (susp_meta:meta_node)
        (user_function:unit->Data.t) : Data.t susp_state * dcg_state
        =
      let viznode = susp_meta.viznode in
      Viz.tick (Viz.Eval_pre viznode);
      incr Statistics.Counts.evaluate;
      if Params.dirty_exactly then begin
        match !shared_susp_node with
        | Empty -> ()
        | Vacant _ -> ()
        | Prenode _ -> ()
        | Node sn -> susp_node_undo_edges sn false
      end ;
      let stack_frame = { stkf_edge_src = susp_meta ;
                          stkf_obs_edges = [] ;
                          stkf_mut_edges = [] } in
      force_stack := stack_frame :: !force_stack ;
      let value = try
                    Data.sanitize ( user_function () )
        with exn ->
          force_stack := List.tl !force_stack;
          raise exn
      in
      force_stack := List.tl !force_stack;
      let forces    = List.rev stack_frame.stkf_obs_edges in
      let mutations = List.rev stack_frame.stkf_mut_edges in
      let dcg_state1 = dcg_state_of_forces forces in
      let dcg_state2 = dcg_state_of_mutations mutations in            
      (* all-clean-out assertion. Checks that the user program's run
         is "monotonic", meaning that it does not overwrite (and change)
         the part of the graph that it writes or reads from. *)
      (
        if ((dcg_state1 = Consistent) && (dcg_state2 = Consistent)) then
          ()
        else
          Printf.eprintf "warning: Unexpected DCG inconsistency (are you using names correctly? are you using `set` correctly?)\n%!"
      ) ;
      (*C
        ( if dcg_state1 = Consistent && dcg_state2 = Consistent
        then Viz.tick (Viz.Comment "all-clean-out")
        else Viz.tick (Viz.Comment "not all-clean-out") ) ;
      *)
      let _ = IFDEF ADAPTON_LOG THEN (
        let force_tgts : (Viz.node * (unit -> Viz.flag)) list =
          List.map (fun dep -> (dep.dependee.viznode, fun () -> pv_of_flag dep.flag))
            forces
        in
        let mut_tgts : (Viz.node * (unit -> Viz.flag)) list =
          List.map (fun dep -> (dep.mut_target.viznode, fun () -> pv_of_flag dep.mut_flag))
            mutations
        in
        viznode.Viz.force_tgts <- force_tgts ;
        viznode.Viz.mut_tgts   <- mut_tgts ;
        viznode.Viz.val_string <- (fun () -> Data.string value) ;
        Viz.tick (Viz.Eval_ret(viznode, Data.string value));
        Viz.tick (Viz.Eval_post viznode);
      ) END
      in
      (* repair/receipt performs a truncated inorder traversal of the dependency graph *)
      let repair cont  =
        match !shared_susp_node with
        | Empty -> failwith "impossible: case Empty is excluded by check logic"
        | Vacant _ -> failwith "impossible: case Vacant _ is excluded by check logic"
        | Prenode _ -> failwith "impossible: case Prenode _ is excluded by check logic"
        | Node sn -> (
          Viz.tick (Viz.Clean_pre viznode) ;
          let value    = sn.susp_state.susp_value () in
          let receipt  = sn.susp_state.susp_receipt in
          let forces   = sn.susp_state.susp_forces in
          let evaluate = sn.susp_state.susp_evaluate in              
          
          let re_evaluate () =
            if sn.susp_meta.meta_state <> Ok then (
                  (* Re-evaluate this node. *)
              Viz.tick (Viz.Mark_filthy(sn.susp_meta.viznode,false));
              sn.susp_meta.meta_state <- Ok ;
            ) ;
            let (value,receipt,dcg_state) = evaluate () in
            cont (value,receipt,dcg_state)
          in
          
          let rec repair_deps ds = 
            match ds with
            | [] -> cont ( value, receipt, ( dcg_state_of_forces forces ) )                      
            | d::ds ->
              if d.flag <> Dirty then (
                match d.flag with
                | Clean -> repair_deps ds
                | Obsolete_edge -> re_evaluate () (* Due to eviction. *)
                | Dirty_to_clean -> re_evaluate () (* ??? / XXX *)
                | Dirty -> failwith "impossible"
              ) 
              else ( 
                assert ( d.flag = Dirty ) ;
                d.flag <- Dirty_to_clean ;
                IFDEF ADAPTON_LOG THEN (
                  Viz.tick (Viz.Clean_edge_pre(d.dependent.viznode,d.dependee.viznode));
                ) END ;
                let cxt =
                  d.receipt.check ( (* calls repair method on this dependency. *)
                    fun (result_is_unchanged, dcg_state) ->
                      if dcg_state = Maybe_inconsistent then
                        d.flag <- Dirty ;
                      IFDEF ADAPTON_LOG THEN (
                        let edge = (d.dependent.viznode,d.dependee.viznode) in
                        Viz.tick (Viz.Check_edge(fst edge, snd edge, result_is_unchanged));
                        begin match d.flag with
                        | Dirty_to_clean -> Viz.tick (Viz.Clean_edge_post (fst edge, snd edge));
                        | Clean ->          Viz.tick (Viz.Clean_edge_post (fst edge, snd edge));
                        | Obsolete_edge ->  Viz.tick (Viz.Clean_edge_post_obsolete (fst edge, snd edge));
                        | Dirty ->          Viz.tick (Viz.Clean_edge_post_dirty (fst edge, snd edge));
                        end
                      ) END ;
                      begin match d.flag with
                      | Dirty_to_clean -> ( d.flag <- Clean ; incr Statistics.Counts.clean )
                      | Obsolete_edge  -> ()
                      | Dirty          -> ()
                      | Clean          -> failwith "expected: Dirty_to_clean, not Clean"
                      end ;                              
                      if result_is_unchanged then
                        repair_deps ds
                      else
                        re_evaluate ()
                  )                            
                in
                cxt
              )
          in
          if sn.susp_meta.meta_state = Ok then 
                (* Loop over outgoing force edges, repairing each one: *)
            repair_deps forces
          else (
            re_evaluate ()
          )        
        )
      in
      let check cont =
        match Params.check_receipt, (!shared_susp_node) with
        | false, _        -> cont (false, Maybe_inconsistent)
        | true, Vacant _  -> cont (false, Maybe_inconsistent)
        | true, Empty     -> failwith "check: impossible: Cannot be Empty"
        | true, Prenode _ -> failwith "check: impossible: Cannot be Prenode _"
        | true, Node sn -> (
          (sn.susp_state.susp_repair).repair (
            fun (value', _receipt, dcg_state)->
              let isequal = Data.equal value value' in
              IFDEF ADAPTON_LOG THEN (
                Viz.tick (Viz.Check(sn.susp_meta.viznode,
                                    Data.string value,
                                    Data.string value',
                                    isequal));
              ) END ;
              cont (isequal, dcg_state)
           ))
      in
      let re_evaluate : Data.t evaluate = fun () ->
        match (!shared_susp_node) with
        | Empty     -> failwith "re_evaluate: impossible: Cannot be Empty"
        | Prenode _ -> failwith "re_evaluate: impossible: Cannot be Prenode _"
        | Vacant x ->
          let (_meta, value, receipt, dcg_state, _is_fresh_node) =
            susp_ptr_force x.vac_susp
          in
          (value, receipt, dcg_state)

        | Node sn ->
          let ss, dcg_state = susp_evaluate shared_susp_node
            sn.susp_meta user_function in
          sn.susp_state <- ss ;
          ( ss.susp_value (), ss.susp_receipt, dcg_state )
      in
      ( { susp_evaluate=re_evaluate;
          susp_receipt ={check};
          susp_repair  ={repair};
          susp_value   =(fun ()->Data.sanitize value);
          susp_forces  =forces;
          susp_creates =mutations }, 
        dcg_state_meet dcg_state1 dcg_state2 )

    module Unit_arg = struct
      type t = unit
      let equal _ _ = false
      let string _ = "()"
      let hash seed _ = seed
      let sanitize () = ()
    end

    let thunk (nm:Name.t) (user_function : unit -> 'a) : 'a node =
      (* XXX: Use name. *)
      incr Statistics.Counts.create;
      let meta_node = make_meta () in
      let ssn = ref (Prenode{prenode_meta=meta_node;
                             prenode_undo=(fun () -> ());
                             prenode_refc=0}) in
      let module Susp = struct
        module Mfn = struct
          module Name = Name
          module Arg = Unit_arg
          let name () = Name.string nm
          let id = meta_node.id
          type res = Data.t mfn_res
        end
        type t = unit
        let get_key () = `Arg () (* TODO: Fixme. *)
        let get_arg () = ()
        let set_arg () () = ()
        let set_id () _ = ()
        let get_key_hash () = 0
        let get_res () = fun sp ->
          meta_node, begin fun () ->
            let ss, dcg_state = 
              ( susp_evaluate ssn meta_node user_function ) 
            in
            let create_node () =
              sp.susp_node <- ssn ;
              let sn = {susp_meta=meta_node;
                        susp_state=ss;
                        susp_refc=(refc_of_shared_susp_node ssn);
                        susp_undo=nil_undo} in
              ssn := (Node sn) ;
              IFDEF ADAPTON_LOG THEN (
                let viznode = meta_node.viznode in
                viznode.Viz.refc <- (fun () -> Some sn.susp_refc);
                viznode.Viz.arg_string <- (fun () -> "()") ;
              ) END;
            in
            let dcg_state = match !ssn with
              | Empty      -> failwith "thunk/get_res: impossible: Cannot be Empty"
              | Node _     -> failwith "thunk/get_res: impossible: Cannot be Node _"
              | Vacant x   -> Maybe_inconsistent
              | Prenode pn -> create_node () ; dcg_state
            in
            ( ss.susp_value (), ss.susp_receipt, dcg_state )
          end
      end in
      let sp = {
        susp_node=ssn;
        susp_susp=(module (struct
          module Susp = Susp
          let susp = ()
        end)); } in
      (* XXX: No memo table ==> nothing to evict. *)
      (* let module E = (val !eviction_policy) in E.create sp meta_node ssn ; *)
      Viz.tick (Viz.Create ((force_src()).viznode, meta_node.viznode, true));
      (Susp_ptr sp)

    (* This is a memo table of strong pointers. We perform explicit
       reference counting on the elements to purge unused entries. *)
    module Memotable_make (Helm:Hashtbl.HashedType) : sig
      type t
      val create : int -> t
      val merge  : t -> Helm.t -> Helm.t
      val remove : t -> Helm.t -> unit
      val mem    : t -> Helm.t -> bool
      val find   : t -> Helm.t -> Helm.t
      val fold   : t -> 'a -> ('a -> Helm.t -> 'a) -> 'a
      val length : t -> int
    end = struct
      module Ht = Hashtbl.Make(Helm)
      type t    = Helm.t Ht.t
      let create size   = Ht.create size
      let mem ht elm    = Ht.mem ht elm
      let remove ht elm = Ht.remove ht elm
      let find ht elm   = Ht.find ht elm
      let length ht     = Ht.length ht
      let fold ht (a:'a) (f:'a -> Helm.t -> 'a) =
        Ht.fold (fun elm _ a -> f a elm) ht a
      let merge ht elm =
        if Ht.mem ht elm then
          Ht.find ht elm
        else
          ( Ht.add ht elm elm ; elm )
    end

    let mk_mfn (type a)
        (nm:Name.t) (* XXX: Use name. *)
        (module Arg : DatType with type t = a)
        (user_function: Arg.t mfn -> Arg.t -> Data.t) 
        : Arg.t mfn
      =
      let fresh_seed = Random.bits () in

      let module Memotable_row = struct
        type t = Data.t susp_ptr
        let equal sp1 sp2 = susp_ptr_equal sp1 sp2
        let hash sp       = susp_ptr_hash fresh_seed sp
      end in

      let module Memotable = Memotable_make (Memotable_row) in

      let fresh_id = !next_id in incr next_id ;
      let memotable = Memotable.create 0 in
      let numadd = ref 0 in
      let numrem = ref 0 in
      incr Statistics.Counts.tables ;

      let module Memotable = struct
        include Memotable
        let remove mt elm =
          incr numrem ; remove mt elm
      end in

      let module Mfn = struct
        module Arg  = Arg
        module Name = Name
        let name () = Name.string nm
        type res = Data.t mfn_res
        let id = fresh_id
      end in

      Memotables.register
        ( module struct
          module Mfn = Mfn
          let length () = Memotable.length memotable
          let numadd () = !numadd
          let numrem () = !numrem
          let fold a f  = Memotable.fold memotable a
            (fun a sp -> f a (shared_susp_node_meta sp.susp_node).viznode)
        end )
      ;

      let module M = struct
        let mfn_nm = nm

        (* This module is defined in terms of Memotable, Mfn and the
           user_function. *)
        module rec Rec : sig
          (* Create memoized thunks: *)
          val create_susp_ptr_arggen : Arg.t -> Data.t susp_ptr
          val create_susp_ptr_arg    : Arg.t -> Data.t susp_ptr
          val create_susp_ptr_nm     : Name.t -> Arg.t -> Data.t susp_ptr
          (* Get results of memoized thunks: *)
          val mfn_res_of_susp_ptr_without_meta : Arg.t -> Data.t mfn_res
          val mfn_res_of_susp_ptr_with_meta    : (unit -> Arg.t) -> Data.t mfn_res
          (* Get the programmer API defined below: *)
          val get_mfn : unit -> Arg.t mfn (* Recursive module; must be function type. *)
        end = struct

          let rec mfn = (* Programming API for memoized recursion: *)
            { mfn_data = (fun arg -> user_function mfn arg) ;
              mfn_art  = (fun arg -> 
                if Params.disable_mfns then
                  cell (Name.nondet()) (user_function mfn arg)
                else
                  if Params.generative_ids then
                    Susp_ptr(Rec.create_susp_ptr_arggen arg)
                  else
                    Susp_ptr(Rec.create_susp_ptr_arg arg)
              ) ;
              mfn_nart = (fun nm arg -> 
                if Params.disable_mfns then
                  cell (Name.nondet()) (user_function mfn arg)
                else
                  if Params.disable_names then (
                    if Params.generative_ids then
                      Susp_ptr(Rec.create_susp_ptr_arggen arg)
                    else
                      Susp_ptr(Rec.create_susp_ptr_arg arg)
                  )
                  else
                    Susp_ptr(Rec.create_susp_ptr_nm nm arg)
              );
            }
          let get_mfn () = mfn
            
          (* Generative equality/identity; 
             Emulates Adapton Classic. *)
          module Susp_arggen = struct
            module Mfn = Mfn
            type t = { arg      : Arg.t ;
                       arg_hash : int ;
                       mutable id : int option ; }
            let get_key x      = `ArgGen( x.arg,  x.id )
            let get_arg x      = x.arg
            let set_arg _ _    = failwith "set_arg: Cannot change argument."
            let set_id  x id   = assert (x.id = None) ; x.id <- (Some id)
            let get_key_hash x = x.arg_hash
            let get_res x sp   = (Rec.mfn_res_of_susp_ptr_with_meta 
                                    (fun () -> get_arg x) sp)
          end

          (* Structural equality/identity. *)
          module Susp = struct 
            module Mfn = Mfn
            type t = { arg      : Arg.t ;
                       arg_hash : int ; }
            let get_key x      = `Arg x.arg
            let get_arg x      = x.arg
            let set_arg _ _    = failwith "set_arg: Cannot change argument."
            let set_id  x id   = failwith "set_id: Cannot change id: There is no id."
            let get_key_hash x = x.arg_hash
            let get_res x      = Rec.mfn_res_of_susp_ptr_without_meta x.arg
          end

          (* Nominal equality/identity. *)
          module Susp_nm_make( X : sig val name : Name.t
                                       val arg  : Arg.t end) = struct
            module Mfn = Mfn              
            let arg_cell = ref (Arg.sanitize X.arg)
            let name_hash = Name.hash fresh_seed X.name
            type t = unit
            let get_key ()      = `Name X.name
            let get_arg ()      = !arg_cell
            let set_arg () arg  = arg_cell := (Arg.sanitize arg)
            let set_id  x id    = failwith "set_id: Cannot change id: There is no id."
            let get_key_hash () = name_hash
            let get_res () sp   = Rec.mfn_res_of_susp_ptr_with_meta get_arg sp
          end
                
          let create_create_edge ~is_fresh:(is_fresh:bool) (sp:'a susp_ptr) : unit =
            let ssn = sp.susp_node in
            let meta_node = meta_of_shared_susp_node ssn in
            let curr_refc = refc_of_shared_susp_node ssn in
            ( match !force_stack with
            | [] -> (* Empty force stack implies creator is external user code. *)
              Viz.tick (Viz.Create ((force_src()).viznode, meta_node.viznode, is_fresh));
              if Params.ref_count then (
                incr_refc_of_shared_susp_node ssn ;
                let module E = (val !eviction_policy) in E.refc_incr sp ;
                Viz.tick (Viz.Refc_incr (meta_node.viznode, curr_refc+1)) ;
                Gc.finalise (fun sp -> susp_node_decr_refc sp false) sp
              )
                
            | stkf :: _ ->
              Viz.tick (Viz.Create (stkf.stkf_edge_src.viznode, meta_node.viznode, is_fresh));
              let mut_edge' =
                { mut_flag=Clean;
                  mut_source=stkf.stkf_edge_src;
                  mut_target=(IFDEF ADAPTON_LOG THEN meta_node ELSE () END);
                  mut_undo=
                    if Params.ref_count then (
                      incr_refc_of_shared_susp_node ssn ;
                      let module E = (val !eviction_policy) in E.refc_incr sp ;
                      Viz.tick (Viz.Refc_decr (meta_node.viznode, curr_refc+1)) ;
                      (fun undo_now -> susp_node_decr_refc sp undo_now)
                    ) else 
                      nil_undo ; }
              in
              let mut_edge = Mutators.merge meta_node.mutators mut_edge' in
              stkf.stkf_mut_edges <- mut_edge :: stkf.stkf_mut_edges ;
            )

          let debug_assert_check () =
            if Params.debug_assert then (
              List.iter (
                  fun stkf ->
                  List.iter (
                      fun dep ->
                      assert( dep.flag = Clean );
                    ) stkf.stkf_obs_edges ;
                  List.iter (
                      fun dep ->
                      assert( dep.mut_flag = Clean );
                    ) stkf.stkf_mut_edges ;
                )
                (!force_stack)
            )

          let create_susp_ptr_nm (nm:Name.t) (arg:Arg.t) : Data.t susp_ptr =
            let create_shared_susp_node_nm sp : unit = 
              let meta_node = make_meta () in
              let ssn = sp.susp_node in
              let undo () = Memotable.remove memotable sp in
              ssn := Prenode {prenode_meta = meta_node;
                              prenode_undo = undo;
                              prenode_refc = (refc_of_shared_susp_node ssn) } 
              ;
              (* {{ Eviction, Viz *)
              let module E = (val !eviction_policy) in E.create sp meta_node ssn ;
              Viz.tick (Viz.Create ((force_src()).viznode, meta_node.viznode, true));
              IFDEF ADAPTON_LOG THEN (
                let viznode = meta_node.viznode in
                let module S = (val sp.susp_susp : SuspValType 
                                with type Susp.Mfn.res = Data.t mfn_res) in
                viznode.Viz.mfn_string <- (fun () -> Name.string mfn_nm) ;
                viznode.Viz.name_string <- (fun () -> Name.string nm) ;
                viznode.Viz.refc <- (fun () -> Some (refc_of_shared_susp_node ssn)) ;                
                viznode.Viz.arg_string <- (fun () -> Arg.string (Obj.magic (S.Susp.get_arg S.susp))) ;
              ) END;
              (* }} *)
            in
            let rec sp_new = { 
              susp_node=ref Empty;
              susp_susp=(module (struct
                module Susp = Susp_nm_make(struct let name = nm
                                                  let arg  = arg end)
                let susp = ()
              end )); } 
            in              
            (* Do memo table lookup. Dirty things if needbe. *)
            let sp0 = Memotable.merge memotable sp_new in
            if sp0 == sp_new then (
              (* {{ Stats *)
              incr Statistics.Counts.create;
              incr Statistics.Counts.miss;
              incr numadd ;
              (* }} *)
              create_shared_susp_node_nm sp_new ;
              create_create_edge ~is_fresh:true sp_new ;
              sp_new
            ) else (
              (* {{ Stats *)
              incr Statistics.Counts.hit;
              (* }} *)
              let ssn0 = sp0.susp_node in              
              let share_existing_susp_ptr () = 
                let module S = (val sp0.susp_susp : SuspValType
                                with type Susp.Mfn.res = Data.t mfn_res) in
                let force_src_meta = force_src () in
                let sp0_meta_node = meta_of_shared_susp_node ssn0 in
                if not (Arg.equal arg (Obj.magic (S.Susp.get_arg S.susp))) then (
                  Mutators.fold ( fun edge () ->
                    if ( not (edge.mut_source == force_src_meta)
                         && not (meta_is_root force_src_meta)
                         && edge.mut_flag <> Obsolete_edge ) then (
                      mark_filthy edge.mut_source ;
                      debug_assert_check () ;
                    )
                    else ()
                  ) sp0_meta_node.mutators () ;
                  S.Susp.set_arg S.susp (Obj.magic arg) ;
                  mark_filthy sp0_meta_node ;
                  debug_assert_check () ;
                ) ;
                create_create_edge ~is_fresh:false sp0 ;
                (* {{ Eviction, Viz *)
                (* let module E = (val !eviction_policy) in E.share sp0 sn ; *) (* TODO: Get the sn out, or do not pass at all. *)
                Viz.tick (Viz.Create ((force_src()).viznode, sp0_meta_node.viznode, false));
                (* }} *)
              in
              ( match !ssn0 with
              | Empty     -> failwith "Expected: Node _, not Empty"
              | Vacant _  -> failwith "Expected: Node _, not Vacant _"
              (* Or.. allow vacant nodes in memo table and we use create_shared_susp_node_nm () ? *)              
              | Prenode _ | Node _ -> 
                ( share_existing_susp_ptr () ; sp0 )
              )
            )

          let create_susp_ptr_arggen (arg:Arg.t) : Data.t susp_ptr =
            let create_shared_susp_node_arggen sp : unit = 
              let meta_node = make_meta () in
              let ssn = sp.susp_node in
              let undo () = Memotable.remove memotable sp in
              let module S = (val sp.susp_susp : SuspValType
                              with type Susp.Mfn.res = Data.t mfn_res) in
              S.Susp.set_id S.susp meta_node.id ;
              ssn := Prenode {prenode_meta = meta_node;
                              prenode_undo = undo;
                              prenode_refc = (refc_of_shared_susp_node ssn) } 
              ;
              (* {{ Eviction, Viz *)
              let module E = (val !eviction_policy) in E.create sp meta_node ssn ;
              Viz.tick (Viz.Create ((force_src()).viznode, meta_node.viznode, true));
              IFDEF ADAPTON_LOG THEN (
                let viznode = meta_node.viznode in
                let module S = (val sp.susp_susp : SuspValType 
                                with type Susp.Mfn.res = Data.t mfn_res) in
                viznode.Viz.mfn_string <- (fun () -> Name.string mfn_nm) ;
                viznode.Viz.name_string <- (fun () -> "?") ;
                viznode.Viz.refc <- (fun () -> Some (refc_of_shared_susp_node ssn)) ;                
                viznode.Viz.arg_string <- (fun () -> Arg.string (Obj.magic (S.Susp.get_arg S.susp))) ;
              ) END;
              (* }} *)
            in
            let arg = Arg.sanitize arg in
            let sp_new = { 
              susp_node=ref Empty;
              susp_susp=(module (struct
                module Susp = Susp_arggen
                let susp = { Susp_arggen.arg = arg;
                             Susp_arggen.arg_hash = Arg.hash fresh_seed arg;
                             Susp_arggen.id = None ; }
              end )); }
            in
            (* Do memo table lookup. 
               Since id of sp_new is None, lookup is based on the argument. *)
            let sp0 = Memotable.merge memotable sp_new in
            if sp0 == sp_new then (
              (* {{ Stats *)
              incr Statistics.Counts.create;
              incr Statistics.Counts.miss;
              incr numadd ;
              (* }} *)
              create_shared_susp_node_arggen sp_new ;
              create_create_edge ~is_fresh:true sp_new ;
              sp_new
            ) else (
              (* {{ Stats *)
              incr Statistics.Counts.hit;
              (* }} *)
              let module S = (val sp0.susp_susp : SuspValType
                              with type Susp.Mfn.res = Data.t mfn_res) in
              assert (Arg.equal arg (Obj.magic (S.Susp.get_arg S.susp))) ;
              ( match !(sp0.susp_node) with
              | Empty     -> failwith "Expected: Node _, not Empty"
              | Vacant _  -> failwith "Expected: Node _, not Vacant _"
              | Prenode _ | Node _ -> 
                create_create_edge ~is_fresh:false sp0 ;
                (* {{ Eviction, Viz *)
                (* let module E = (val !eviction_policy) in E.share sp0 sn ; *) (* TODO: Get the sn out, or do not pass at all. *)
                let ssn0 = sp0.susp_node in
                let sp0_meta_node = meta_of_shared_susp_node ssn0 in
                Viz.tick (Viz.Create ((force_src()).viznode, sp0_meta_node.viznode, false));
                (* }} *)
                sp0
              )
            )

          (* TODO: Rename "mfn_res_of_susp_ptr_with_meta" ? *)
          let mfn_res_of_susp_ptr_with_meta (get_arg: (unit -> Arg.t)) : Data.t mfn_res = 
            fun (sp:Data.t susp_ptr) ->
              let ssn = sp.susp_node in
              let meta_node = shared_susp_node_meta ssn in
              meta_node, (fun () ->
                let ss, dcg_state = 
                  ( susp_evaluate ssn meta_node
                      (fun () -> user_function mfn (Arg.sanitize (get_arg ()))) )
                in
                let sn = {susp_meta  = meta_node;
                          susp_state = ss;
                          susp_refc  = (refc_of_shared_susp_node ssn);
                          susp_undo  = (undo_of_shared_susp_node ssn)} in
                ssn := Node sn ;
                IFDEF ADAPTON_LOG THEN (
                  let viznode = meta_node.viznode in
                  viznode.Viz.refc <- (fun () -> Some sn.susp_refc) ;
                  viznode.Viz.arg_string <- (fun () -> Arg.string (get_arg ())) ;
                ) END;
                (ss.susp_value (), ss.susp_receipt, dcg_state)
              )
              
          let create_susp_ptr_arg (arg:Arg.t) : Data.t susp_ptr =
            let arg = Arg.sanitize arg in
            { susp_node=ref Empty;
              susp_susp=(module (struct
                module Susp = Susp
                let susp = { Susp.arg = arg;
                             Susp.arg_hash = Arg.hash fresh_seed arg; } 
              end )); }
                
          let mfn_res_of_susp_ptr_without_meta (arg:Arg.t) : Data.t mfn_res = 
            fun (sp:Data.t susp_ptr) ->
              let sp_copy = susp_ptr_copy sp in (* XXX: This copy step may no longer be necessary. *)
              let sp0 = Memotable.merge memotable sp_copy in
              if sp0 == sp_copy then begin
              (* Case: Cache Miss. *)
                incr Statistics.Counts.create;
                incr Statistics.Counts.miss;
                incr numadd ;
                let meta_node = make_meta () in
                meta_node, begin fun () ->
                  let ssn = sp.susp_node in
                  let module E = (val !eviction_policy) in E.create sp meta_node ssn ;
                  Viz.tick (Viz.Create ((force_src()).viznode, meta_node.viznode, true));
                  let ss, dcg_state = 
                    ssn := (Prenode {prenode_meta = meta_node;
                                     prenode_undo=(fun () -> ());
                                     prenode_refc = (refc_of_shared_susp_node ssn) }) ;
                    ( susp_evaluate ssn meta_node
                        (fun () -> user_function mfn (Arg.sanitize arg)))
                  in
                  let undo () = Memotable.remove memotable sp in
                  let sn = {susp_meta  = meta_node;
                            susp_state = ss;
                            susp_refc  = (refc_of_shared_susp_node ssn);
                            susp_undo  = undo} in
                  ssn := Node sn ;
                  IFDEF ADAPTON_LOG THEN (
                    let viznode = meta_node.viznode in
                    viznode.Viz.refc <- (fun () -> Some sn.susp_refc) ;
                    viznode.Viz.arg_string <- (fun () -> Arg.string arg) ;
                  ) END;
                  (ss.susp_value (), ss.susp_receipt, dcg_state)
                end
              end
              else begin
              (* Case: Cache Hit. *)
                incr Statistics.Counts.hit;
                let ssn0 = sp0.susp_node in
                if not (sp.susp_node == ssn0) then (
                  let sp_refc = refc_of_shared_susp_node sp.susp_node in
                  sp.susp_node <- ssn0 ;
                  match !ssn0 with
                  | Node sn -> (sn.susp_refc <- sn.susp_refc + sp_refc)
                  | _ -> failwith "Expected: Memo hit should be a Node _"
                ) ;
                begin match !ssn0 with
                | Empty     -> failwith "Expected: Node _, not Empty"
                | Vacant _  -> failwith "Expected: Node _, not Vacant _"
                | Prenode _ -> failwith "Expected: Node _, not Prenode _"
                | Node sn ->
                  let module E = (val !eviction_policy) in E.share sp sn ;
                  Viz.tick (Viz.Create ((force_src()).viznode, sn.susp_meta.viznode, false));
                  sn.susp_meta, begin fun () ->
                    sn.susp_state.susp_repair.repair
                      (fun value_receipt -> value_receipt) ;
                  end
                end
              end
                
          end (* Rec *)
          include Rec
        end (* M *)
      in
      M.get_mfn ()

  end (* Make *)

  module Eviction : Primitives.EvictionType = struct

    let max_node_limit = ref None

    let flush () = flush ()

    let limit_string () = match !max_node_limit with
      | None -> "None"
      | Some n -> Printf.sprintf "(Some %d)" n

    let set_limit limit =
      assert ( match limit with None -> true | Some n -> n >= 0 ) ;
      max_node_limit := limit

    module Evict_fifo : Eviction_policy = struct
      let string () = (Printf.sprintf "Evict_fifo %s" (limit_string ()))
      let q = Queue.create ()
      let purge () =
        let rec loop () =
          match !max_node_limit with
          | Some limit when Queue.length q > limit ->
            let sp = Queue.pop q in
            susp_ptr_evict (Obj.magic sp) ; loop ()
          | _ -> ()
        in loop ()
      let create sp meta ssn = Queue.push (Obj.repr sp) q ; 
        if Params.eviction_time = `On_create then purge ()
      let share sp ssn = ()
      let force sp sn = ()
      let refc_incr sp = ()
      let refc_decr sp = ()
      let flush () = purge ()
    end

    module Evict_lru : Eviction_policy = struct
      let string () = (Printf.sprintf "Evict_lru %s" (limit_string ()))
      type ts_node = int * meta_node * Obj.t (* Time-stamped nodes *)
      let ts_node_cmp (x,_,_) (y,_,_) = x - y (* least recent timestamp. *)
      module H = Core.Heap.Removable
      module M = Map.Make(struct type t = int
                                 let compare x y = x - y
      end)
      let h = H.create ~cmp:ts_node_cmp () (* priority queue, as a heap ordered by time stamps. *)
      let m = ref M.empty (* mapping from node IDs to heap elements. *)
      let clock = ref 0 (* Clock maintains current time stamp. *)

      let tick is_create meta sp = (* Advance the clock; update node to carry latest time. *)
        if M.mem meta.id (!m) then begin
          let hel = M.find meta.id (!m) in
          let hel = H.update h hel (!clock, meta, Obj.repr sp) in
          m := (M.add meta.id hel (!m)) ;
          incr clock ;
        end
        else if is_create then begin
          let hel = H.add_removable h (!clock, meta, Obj.repr sp) in
          m := (M.add meta.id hel (!m)) ;
          incr clock
        end
        else
          ()

      let purge () = (* Remove nodes with the earliest stamps until limit is respected. *)
        let rec loop () =
          match !max_node_limit with
          | Some limit when H.length h > limit ->
            (match H.top h with
            | None -> ()
            | Some (ts, meta_node, sp) ->
              H.remove_top h ;
              m := M.remove meta_node.id (!m) ;
              susp_ptr_evict (Obj.magic sp) ;
              loop ()
            )
          | _ -> ()
        in loop ()
      let count = ref 0
      let create sp meta ssn = tick true meta sp ;
        if Params.eviction_time = `On_create then purge ()
      let share sp sn = tick false sn.susp_meta sp
      let force sp sn = tick false sn.susp_meta sp
      let refc_incr sp = ()
      let refc_decr sp = ()
      let flush () = purge ()
    end

    let _ =
      eviction_policy :=
        match Params.eviction_policy with
        | `None -> (module No_eviction_policy)
        | `Fifo limit -> max_node_limit := (Some limit) ;
          (module Evict_fifo)
        | `Lru limit -> max_node_limit := (Some limit) ;
          (module Evict_lru)

    let set_policy name =
      if true then
        failwith "Deprecated: Use functor parameter instead: Params.eviction_policy."
      else
        eviction_policy :=
          match name with
          | "none" | "NONE" -> (module No_eviction_policy)
          | "fifo" | "FIFO" -> (module Evict_fifo)
          | "lru"  | "LRU"  -> (module Evict_lru)
          | _ -> failwith "unrecognized eviction policy name"
  end

  module ArtLib : GrifolaType.ArtLibType = struct
    type lib_id
    module Memotables = Memotables
    module Eviction = Eviction
    module MakeArt = MakeArt

    module MakeArtTuple2
      (Name:GrifolaType.NameType) 
      (Art1:GrifolaType.ArtType with type Name.t = Name.t) 
      (Art2:GrifolaType.ArtType with type Name.t = Name.t) 
      : GrifolaType.ArtTuple2Type
      with type Name.t = Name.t
      and  type Art1.t = Art1.t and type Art1.Data.t = Art1.Data.t and type Art1.Name.t = Name.t
      and  type Art2.t = Art2.t and type Art2.Data.t = Art2.Data.t and type Art1.Name.t = Name.t =
    struct
      module Name = Name
      module Art1 = Art1
      module Art2 = Art2
      module Art = MakeArt(Name)(AdaptonTypes.Tuple2(Art1.Data)(Art2.Data))        
      
      let mfn_fst = Art1.mk_mfn (Name.gensym "fst") (module Art) (fun r art -> fst (Art.force art))
      let mfn_snd = Art2.mk_mfn (Name.gensym "snd") (module Art) (fun r art -> snd (Art.force art))

      let fst nm art = if true then mfn_fst.Art1.mfn_art art else mfn_fst.Art1.mfn_nart nm art
      let snd nm art = if true then mfn_snd.Art2.mfn_art art else mfn_snd.Art2.mfn_nart nm art

      let split nm x = let nm1,nm2 = Name.fork nm in (fst nm1 x, snd nm2 x)
    end
  end

end (* Grifola module *)

module Default_params = struct
  let eviction_policy   = `None
  let eviction_time     = `On_flush
  let ref_count         = true
  let dirty_exactly     = true
  let check_receipt     = true
  let sanitize_pointers = true
  let disable_names     = false
  let generative_ids    = false
  let disable_mfns      = false
  let debug_assert      = false
end

let params = [
  (* "Adapton Classic" *)
  ( "Grifola_arggen", (module (struct
    include Default_params
    let disable_names  = true
    let generative_ids = true
  end ) : AParamsType ));

  (* "Nominal Adapton" *)  
  ( "Grifola_name", (module (struct
    include Default_params
  end ) : AParamsType ));
]
 
let old_params = [

  ( "default", (module Default_params : AParamsType ));

  ( "nosanitize", (module (struct
    include Default_params
    let sanitize_pointers = false
  end ) : AParamsType ));

  ( "nocheck", (module ( struct
    include Default_params
    let check_receipt = false
  end ) : AParamsType ));

  ( "nocount", (module ( struct
    include Default_params
    let ref_count = false
  end ) : AParamsType ));

  ( "inexact", (module ( struct
    include Default_params
    let dirty_exactly = false
  end ) : AParamsType ));
  
  ( "fifo+rc", (module (struct
    include Default_params
    let eviction_policy = `Fifo 1000
  end) : AParamsType ));
  
  ( "lru+rc", (module (struct
    include Default_params
    let eviction_policy = `Lru 1000
  end) : AParamsType ));
  
  ( "fifo-rc", (module (struct
    include Default_params
    let eviction_policy = `Fifo 20
    let ref_count = false
  end) : AParamsType ));
  
  ( "lru-rc", (module (struct
    include Default_params
    let eviction_policy = `Lru 20
    let ref_count = false
  end) : AParamsType ));

  ( "fifo+rc", (module (struct
    include Default_params
    let eviction_policy = `Fifo 20
  end) : AParamsType ));

  ( "lru+rc", (module (struct
    include Default_params
    let eviction_policy = `Lru 20
  end) : AParamsType ));
]

(* let variants =
  List.map (fun (name, params) ->
    let module M = Make((val params : AParamsType)) in
    (name, (module M.ATypeImpl : AdaptonUtil.Signatures.AType ))
  ) params
 *)
module Default = Make( Default_params )
