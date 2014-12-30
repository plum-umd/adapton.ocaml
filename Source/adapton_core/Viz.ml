
type flag = [ `Clean | `Dirty | `Dirty_to_clean | `Obsolete ]

IFDEF ADAPTON_LOG THEN
  (* node: record of debugging info is associated with each thunk. *)
  (* should be sufficient for traversing the DCG and creating a picture of it as a graph. *)
type node = {
  id            : int ;
  symbol        : string option ; (* Deprecated. *)
  key           : Key.t option ;  (* Deprecated. *)
  (* Getters: *)
  mutable mfn_string   : unit -> string ;
  mutable name_string  : unit -> string ;
  mutable refc         : unit -> int option ;
  mutable finalizers   : unit -> int ;
  mutable arg_string   : unit -> string ;
  mutable val_string   : unit -> string ;
  mutable is_filthy    : unit -> bool ;
  mutable mut_tgts     : (node * (unit -> flag)) list ;
  mutable force_tgts   : (node * (unit -> flag)) list ;

  (* Currently Unused: ?? *)
  mutable repair_seqno : int ; (* when we last visited the node for repair. *)
  mutable eval_seqno_before: int ; (* when we last decided to re/evaluated the node; updated beforehand. *)
  mutable eval_seqno_after : int ; (* when we last decided to re/evaluated the node; updated afterwards. *)
  mutable check_seqno  : int ; (* when we last checked the node's value. *)
}
ELSE
type node = unit
END

let make_node id key sym =
  IFDEF ADAPTON_LOG THEN begin
     { id=id;
       key=key;
       symbol=sym;
       refc=(fun () -> None);
       finalizers=(fun () -> 0);
       mut_tgts=[];
       force_tgts=[];
       is_filthy=(fun _ -> failwith "is_filthy");
       mfn_string=(fun _ -> "?");
       name_string=(fun _ -> "?");
       arg_string=(fun _ -> "?");
       val_string=(fun _ -> "?");
       repair_seqno=(-1);
       eval_seqno_before=(-1);
       eval_seqno_after=(-1);
       check_seqno=(-1);
     }
  end END


let global_seqno_hook : (unit -> int) ref = ref (fun _ -> -1)
let global_seqno () = (!global_seqno_hook)()

(* - - - - - - - - - - - *)

type event =
  | Create of node * node * bool
  | Force_pre of node * node
  | Force_post of node * node

  | Eval_pre of node
  | Eval_ret of node * string
  | Eval_post of node
  | Refc_incr of node * int
  | Refc_decr of node * int

  | Revert of node

  | Undo_pre of node
  | Undo_post of node
  | Undo_skip of node * int

  | Clean_pre of node
  | Clean_edge_pre of node * node
  | Clean_edge_post of node * node
  | Clean_edge_post_obsolete of node * node
  | Clean_edge_post_dirty of node * node
  | Check of node * string * string * bool
  | Check_edge of node * node * bool

  | Mark_filthy of node * bool
  | Dirty_pre of node
  | Dirty_post of node
  | Dirty_edge of node * node

  | Add_finaliser of node
  | Run_finaliser of node

  | Comment of string

type tick = int * event

let print_tick_oldformat out (tickc,event) =
  IFDEF ADAPTON_LOG THEN begin
  Printf.fprintf out "Change state: (%d): " tickc ;
  let string_of_symbol s = match s with Some s -> s | None -> "?" in
(*  let string_of_key k = match k with Some k -> Key.string k | None -> "?" in*)
  begin match event with
    | Create(src,tgt,is_fresh) ->
        Printf.fprintf out "Create &%d → &%d %s %s %b\n%!" src.id tgt.id
          (string_of_symbol tgt.symbol)
          (tgt.arg_string ())
          is_fresh
        ;
        Printf.fprintf out "edge %d %d create-fresh-%b\n%!" src.id tgt.id is_fresh ;

(*
    | Nominal_thunk(node,arg,is_fresh) ->
        Printf.fprintf out "Nominal_thunk &%d %s %s %s %b\n%!"
          node.id (string_of_symbol node.symbol) (string_of_key node.key) arg is_fresh;
        Printf.fprintf out "%d create-fresh-%b\n%!" node.id is_fresh;
*)
    | Refc_incr (node, refc) ->
        Printf.fprintf out "Refc_incr &%d %d\n%!" node.id refc ;

    | Refc_decr (node, refc) ->
        Printf.fprintf out "Refc_decr &%d %d\n%!" node.id refc ;

    | Revert (node) ->
        Printf.fprintf out "Revert %d\n%!" node.id ;
        Printf.fprintf out "%d revert\n%!" node.id ;

    | Undo_pre node ->
        Printf.fprintf out "Undo_pre &%d\n%!" node.id ;
        Printf.fprintf out "%d undo-pre\n%!" node.id ;

    | Undo_post node ->
        Printf.fprintf out "Undo_post &%d\n%!" node.id ;
        Printf.fprintf out "%d undo-post\n%!" node.id ;

    | Undo_skip (node, refc) ->
        Printf.fprintf out "Undo_skip &%d %d\n%!" node.id refc ;
        Printf.fprintf out "%d undo-skip\n%!" node.id ;

    | Force_pre (src, tgt) ->
        Printf.fprintf out "Force_pre &%d &%d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d force-pre\n%!" src.id tgt.id ;

    | Force_post (src,tgt) ->
        Printf.fprintf out "Force_post &%d &%d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d force-post\n%!" src.id tgt.id ;

    | Mark_filthy(node,state) ->
        Printf.fprintf out "Mark_filthy &%d %b\n%!" node.id state ;
        Printf.fprintf out "%d mark-filthy\n%!" node.id ;

    | Clean_pre node ->
        Printf.fprintf out "Clean_pre &%d\n%!" node.id ;
        Printf.fprintf out "%d clean-pre\n%!" node.id ;

    | Clean_edge_pre(src,tgt) ->
        Printf.fprintf out "Clean_edge_pre &%d --> &%d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d clean-edge-pre\n%!" src.id tgt.id ;

    | Clean_edge_post(src,tgt) ->
        Printf.fprintf out "Clean_edge_post &%d --> &%d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d clean-edge-post\n%!" src.id tgt.id ;

    | Clean_edge_post_obsolete(src,tgt) ->
        Printf.fprintf out "Clean_edge_post_obsolete %d --> %d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d clean-edge-post-obsolete\n%!" src.id tgt.id ;

    | Clean_edge_post_dirty(src,tgt) ->
        Printf.fprintf out "Clean_edge_post_dirty %d --> %d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d clean-edge-post-dirty\n%!" src.id tgt.id ;

    | Eval_pre node ->
        Printf.fprintf out "Eval_pre &%d\n%!" node.id ;
        Printf.fprintf out "%d eval-pre\n%!" node.id ;

    | Eval_ret(node,res) ->
        Printf.fprintf out "Eval_ret &%d %s\n%!" node.id res ;
        Printf.fprintf out "%d eval-ret\n%!" node.id ;

    | Eval_post node ->
        Printf.fprintf out "Eval_post &%d\n%!" node.id ;
        Printf.fprintf out "%d eval-post\n%!" node.id ;

    | Dirty_pre node ->
        Printf.fprintf out "Dirty_pre &%d\n%!" node.id ;
        Printf.fprintf out "%d dirty-pre\n%!" node.id ;

    | Dirty_edge(src,tgt) ->
        Printf.fprintf out "Dirty_edge &%d --> &%d\n%!" src.id tgt.id ;
        Printf.fprintf out "%d %d dirty-edge\n%!" src.id tgt.id ;

    | Dirty_post node ->
        Printf.fprintf out "Dirty_post &%d\n%!" node.id ;
        Printf.fprintf out "%d dirty-post\n%!" node.id ;

    | Check (node,prev,cur,isequal) ->
        Printf.fprintf out "Check &%d %s %s %b\n%!" node.id prev cur isequal ;
        Printf.fprintf out "%d check-%b\n%!" node.id isequal ;

    | Check_edge(src,tgt,isequal) ->
        Printf.fprintf out "Check_edge &%d --> &%d %b\n%!" src.id tgt.id isequal ;
        Printf.fprintf out "%d %d check-edge-%b\n%!" src.id tgt.id isequal ;

    | Add_finaliser node ->
        Printf.fprintf out "Add_finaliser %d\n" node.id ;
        Printf.fprintf out "%d add-finaliser\n%!" node.id ;

    | Run_finaliser node ->
        Printf.fprintf out "Run_finaliser %d\n" node.id ;
        Printf.fprintf out "%d run-finaliser\n%!" node.id ;

    | Comment comment ->
      Printf.fprintf out "Comment: %s\n" comment ;      

  end end ENDIF


let print_tick_newformat out (tickc,event) =
  IFDEF ADAPTON_LOG THEN begin
  Printf.fprintf out "[change]•%d: " tickc ;
    (*
  let string_of_symbol s = match s with Some s -> s | None -> "?" in
  let string_of_key k = match k with Some k -> Key.string k | None -> "?" in
    *)
  let print_node n = 
    Printf.fprintf out "<hr/><b>Node %d</b>\n" n.id ;
    Printf.fprintf out "<i>mfn:</i> %s\n" (n.mfn_string ()) ;
    Printf.fprintf out "<i>name:</i> %s\n" (n.name_string ()) ;
    Printf.fprintf out "<i>arg:</i> %s\n" (n.arg_string ()) ;
    Printf.fprintf out "<i>res:</i> %s\n" (n.val_string ()) ;
    Printf.fprintf out "<i>refc:</i> %s\n" (match n.refc () with Some rc -> string_of_int rc | None -> "None") ;
    Printf.fprintf out "<i>#fin:</i> %d\n" (n.finalizers ()) ;
  in
  begin match event with
    | Create(src,tgt,is_fresh) ->
        Printf.fprintf out "Create %d → %d\n" src.id tgt.id ;
        Printf.fprintf out "[node %d create-fresh-%b]\n%!" tgt.id is_fresh;
        Printf.fprintf out "[edge %d %d create-fresh-%b]\n%!" src.id tgt.id is_fresh;
        print_node tgt ;
        Printf.fprintf out "<i>fresh?:</i> %b\n" is_fresh ;
        print_node src ;

    | Refc_incr (node, refc) ->
        Printf.fprintf out "Refc_incr &%d %d\n%!" node.id refc ;
        Printf.fprintf out "[node %d refc-%d]\n%!" node.id refc ;
        print_node node ;

    | Refc_decr (node, refc) ->
        Printf.fprintf out "Refc_decr &%d %d\n%!" node.id refc ;
        Printf.fprintf out "[node %d refc-%d]\n%!" node.id refc ;
        print_node node ;

    | Revert (node) ->
        Printf.fprintf out "Revert %d\n%!" node.id ;
        Printf.fprintf out "[node %d revert]\n%!" node.id ;
        print_node node ;

    | Undo_pre node ->
        Printf.fprintf out "Undo_pre &%d\n%!" node.id ;
        Printf.fprintf out "[node %d undo-pre]\n%!" node.id ;
        print_node node ;

    | Undo_post node ->
        Printf.fprintf out "Undo_post &%d\n%!" node.id ;
        Printf.fprintf out "[node %d undo-post]\n%!" node.id ;
        print_node node ;

    | Undo_skip (node, refc) ->
        Printf.fprintf out "Undo_skip %d → %d\n%!" node.id refc ;
        Printf.fprintf out "[node %d undo-skip]\n%!" node.id ;
        print_node node ;

    | Force_pre (src, tgt) ->
        Printf.fprintf out "Force_pre %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d force-pre]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Force_post (src,tgt) ->
        Printf.fprintf out "Force_post %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d force-post]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Mark_filthy(node,state) ->
        Printf.fprintf out "Mark_filthy &%d %b\n%!" node.id state ;
        Printf.fprintf out "[node %d mark-filthy]\n%!" node.id ;
        print_node node ;

    | Clean_pre node ->
        Printf.fprintf out "Clean_pre &%d\n%!" node.id ;
        Printf.fprintf out "[node %d clean-pre]\n%!" node.id ;
        print_node node ;

    | Clean_edge_pre(src,tgt) ->
        Printf.fprintf out "Clean_edge_pre %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d clean-edge-pre]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Clean_edge_post(src,tgt) ->
        Printf.fprintf out "Clean_edge_post %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d clean-edge-post]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Clean_edge_post_obsolete(src,tgt) ->
        Printf.fprintf out "Clean_edge_post_obsolete %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d clean-edge-post-obsolete]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Clean_edge_post_dirty(src,tgt) ->
        Printf.fprintf out "Clean_edge_post_dirty %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d clean-edge-post-dirty]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Eval_pre node ->
        Printf.fprintf out "Eval_pre %d\n%!" node.id ;
        Printf.fprintf out "[node %d eval-pre]\n%!" node.id ;
        print_node node ;

    | Eval_ret(node,res) ->
        Printf.fprintf out "Eval_ret %d %s\n%!" node.id res ;
        Printf.fprintf out "[node %d eval-ret]\n%!" node.id ;
        print_node node ;

    | Eval_post node ->
        Printf.fprintf out "Eval_post %d\n%!" node.id ;
        Printf.fprintf out "[node %d eval-post]\n%!" node.id ;
        print_node node ;

    | Dirty_pre node ->
        Printf.fprintf out "Dirty_pre %d\n%!" node.id ;
        Printf.fprintf out "[node %d dirty-pre]\n%!" node.id ;
        print_node node ;

    | Dirty_edge(src,tgt) ->
        Printf.fprintf out "Dirty_edge %d → %d\n%!" src.id tgt.id ;
        Printf.fprintf out "[edge %d %d dirty-edge]\n%!" src.id tgt.id ;
        print_node tgt ;
        print_node src ;

    | Dirty_post node ->
        Printf.fprintf out "Dirty_post %d\n%!" node.id ;
        Printf.fprintf out "[node %d dirty-post]\n%!" node.id ;
        print_node node ;

    | Check (node,prev,cur,isequal) ->
        Printf.fprintf out "Check %d %s %s %b\n%!" node.id prev cur isequal ;
        Printf.fprintf out "[node %d check-%b]\n%!" node.id isequal ;
        print_node node ;

    | Check_edge(src,tgt,isequal) ->
        Printf.fprintf out "Check_edge %d → %d %b\n%!" src.id tgt.id isequal ;
        Printf.fprintf out "[edge %d %d check-edge-%b]\n%!" src.id tgt.id isequal ;
        print_node tgt ;
        print_node src ;

    | Add_finaliser node ->
        Printf.fprintf out "Add_finaliser %d\n" node.id ;
        Printf.fprintf out "[node %d add-finaliser]\n%!" node.id ;
        print_node node ;

    | Run_finaliser node ->
        Printf.fprintf out "Run_finaliser %d\n" node.id ;
        Printf.fprintf out "[node %d run-finaliser]\n%!" node.id ;
        print_node node ;

    | Comment string ->
      Printf.fprintf out "[change] Comment: %s\n%!" string

  end end ENDIF

(* ------------------------------------ *)

let tick_seqno : int ref = ref 0
let tick_buff : tick list ref = ref []

let tick event =
  IFDEF ADAPTON_LOG THEN begin
    let s = !tick_seqno in
    incr tick_seqno ;
    let tick = (s,event) in
    (* print_tick_newformat stderr tick ; *)
    tick_buff := tick :: (!tick_buff)
  end ENDIF

let flush_ticks_out out =
  let ticks = !tick_buff in
  tick_buff := [] ;
  List.iter (print_tick_newformat out) (List.rev ticks) ;
  flush out

let output_graph : ?filename:string -> node -> unit =
  fun ?(filename="") (root:node) -> IFDEF ADAPTON_LOG THEN(
  let module Colors = struct
    (* http://www.graphviz.org/doc/info/colors.html *)
    let hi_blue  = "lavender"    (*"lightblue"*) (*"cornflowerblue"*)
    let lo_blue  = "cornflowerblue"
    let lo_green = "palegreen4"
    let hi_green = "palegreen"
    let red     = "lightpink"   (*"red"*)
    let redgrey = "lightpink4"
    let grey    = "grey"
    let purple  = "mediumorchid1"
  end
  in
  let module S = Set.Make(
    struct
      type t = node
      let compare (d1:node) (d2:node) = d1.id - d2.id
    end )
  in
  let filename =
    if filename <> "" then filename
    else (Printf.sprintf "dcg-%03d-%05d.dot" ((global_seqno())) (!tick_seqno))
  in
  let out =  open_out filename in
  let stack : S.t =
    (* TODO -- get access to the current stack. It is out of scope here. *)
    (*List.fold_right S.add
    (List.map (fun (meta,_,_) -> meta.viznode) (!lazy_stack))*)
    S.empty
  in
  let rec walk (node:node) (visited:S.t) : S.t =
    if S.mem node visited then visited
    else (
      let visited = S.add node visited in
      let label =
        if false (* true means "very descriptive label" *) then
          Printf.sprintf "%d %s\\n%s %s\\n%s"
            node.id
            (match node.key with None -> "" | Some k -> Key.string k)
            (match node.symbol with None -> "" | Some s -> s)
            (node.arg_string ())
            (node.val_string ())
        else
          Printf.sprintf "%d" node.id
      in
      Printf.fprintf out "  n%d [label=\"%s\" color=%s];\n"
        node.id label
        ( if node.is_filthy () && not (S.mem node stack) then Colors.red
          else if node.is_filthy () && (S.mem node stack) then Colors.redgrey (* _Both_ filthy and stacked: Should never see this case. *)
          else if S.mem node stack then Colors.grey
          else if node.check_seqno        = (global_seqno()) then Colors.hi_green
          else if node.eval_seqno_after   = (global_seqno()) then Colors.lo_green
          else if node.eval_seqno_before  = (global_seqno()) then Colors.lo_blue
          else if node.repair_seqno       = (global_seqno()) then Colors.purple
          else Colors.hi_blue )
      ;
      let edge_color_of_flag = function
        | `Clean -> "blue"
        | `Dirty -> "red"
        | `Dirty_to_clean -> "purple"
        | `Obsolete -> "grey"
      in
      let edge_penwidth_of_flag = function
        | `Clean -> 1
        | `Dirty -> 8
        | `Dirty_to_clean -> 8
        | `Obsolete -> 8
      in
      (* dump force and mutation targets of the node: *)
      List.iter begin fun ((tgt:node), edge_flag) ->
        Printf.fprintf out "  n%d -> n%d [color=%s penwidth=%d];\n" node.id tgt.id
          (edge_color_of_flag (edge_flag ()))
          (edge_penwidth_of_flag (edge_flag ()))
      end node.force_tgts ;
      List.iter begin fun ((tgt:node), edge_flag) ->
        Printf.fprintf out "  n%d -> n%d [style=dotted color=%s penwidth=%d];\n"
          node.id tgt.id
          (edge_color_of_flag (edge_flag ()))
          (edge_penwidth_of_flag (edge_flag ()))
          (*(match tgt.key with None -> "" | Some k -> Key.string k) *)
      end node.mut_tgts ;
      (* visit target nodes of edges dumped above: *)
      let visited : S.t =
        List.fold_right
          (fun tgt visited -> (walk tgt visited))
          ((List.map fst node.mut_tgts) @ (List.map fst node.force_tgts)) visited
      in
      visited
    )
  in
  Printf.fprintf out "digraph dcg_%d {\n" ((global_seqno())) ;
  Printf.fprintf out "  labelloc=\"t\";\n" ;
  Printf.fprintf out "  label=\"%s\";\n" filename ;
  Printf.fprintf out "  node [fontname=\"sans-serif\"];\n" ;
  Printf.fprintf out "  node [shape=box style=\"rounded,filled\"];\n" ;
  let visited = S.empty in
  let visited = walk root visited in
  let visited = S.fold (fun node visited -> walk node visited) stack visited in
  ignore visited ;
  Printf.fprintf out "}\n" ;
  close_out out

) ELSE (
  failwith "output_graph: not supported. Recompile with ADAPTON_LOG."
) END


(*
(* A better representation of logs would reflect the steps in the
   flipbook animation, which consists of tree traversals; sketch of
   this below. *)

type sys_ptr = node

type sys_visit = sys_ptr * sys_evt list

and sys_evt =
  | Thunk of Symbol.t option * Key.t option * string
  | Cell  of Symbol.t option * Key.t option * string
  | Force of sys_visit
  | Set   of sys_ptr * string
  | Eval  of sys_visit
  | Dirty of sys_visit
  | Clean of sys_visit
  | Check of sys_ptr * string * string * bool
  | Fork  of Key.t
  | Ret   of string
*)
