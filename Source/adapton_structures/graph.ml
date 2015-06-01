open Adapton_core
open Primitives
open GrifolaType


module RandomInput = struct
  let generate (node_count:int) (edge_count:int)
      : ((int * int) list)         (* (node-id, node-data) list *)
        * ((int * int * int) list) (* (node-id, node-id, edge-data) list *)
    =
    let nodes =
      let rec loop n =
        if n < 0 then []
        else n :: (loop (n-1))
      in
      loop node_count
    in
    let ideal_fract = float_of_int edge_count /.
                        float_of_int (node_count*(node_count-1))
    in
    let edges =
      let rec loop_i nodes_i edges =
        match nodes_i with
        | [] -> edges
        | a :: nodes_i ->
           let rec loop_j nodes_j edges =
             match nodes_j with
             | [] -> loop_i nodes_i edges
             | b :: nodes_j ->
                let edges =
                  let a = Random.int (int_of_float (ideal_fract *. 1000.0)) in
                  let b = Random.int 1000 in
                  if a > b then (a,b,Random.int (10*edge_count)) :: edges
                  else edges
                in
                loop_j nodes_j edges
           in (loop_j nodes edges)
      in loop_i nodes []
    in
    (List.map (fun n -> (n,Random.int (10*node_count))) nodes,
     edges)
end

(*
module Make
         (Name : NameType)
         (ArtLib : ArtLibType)
         (NodeId : DatType)
         (NodeData : DatType)
         (EdgeData : DatType) =
  struct
    module ArtLib = ArtLib
    module Name = Name
    module NodeId = NodeId
    module NodeData = NodeData
    module EdgeData = EdgeData

    module Edge = AdaptonTypes.Tuple3(NodeId)(NodeId)(EdgeData)
    module Adj  = AdaptonTypes.Tuple2(NodeId)(EdgeData)
    module AdjSt = SpreadTree.MakeSpreadTree(ArtLib)(Name)(Adj)
    module NodeVal = AdaptonTypes.Tuple2
                       (AdaptonTypes.Option(NodeData))
                       (AdjSt.List.Data)
                                     
    module NodeMap = Trie.Map.Make(Trie.Useful(NodeId))
                                  (Trie.Useful(NodeVal))
                                  (Name)(ArtLib)

    type t = NodeMap.t

    let empty = NodeMap.empty

    let name_adjs =
      (AdjSt.List.Art.mk_mfn
        (Name.gensym "Graph.name_adjs")
        (module AdjSt.List.Data)
        (fun _ adj_list -> adj_list))
        .AdjSt.List.Art
        .mfn_nart

    let add_node graph nm a a_data =
      match NodeMap.find graph a with
      | None ->
         NodeMap.nadd nm graph a (Some a_data,`Nil)
      | Some (old_data, adjs) ->
         NodeMap.nadd nm graph a (Some a_data, adjs)

    let add_edge graph nm a b ab_data =
      match NodeMap.find graph a with
      | None ->
         NodeMap.nadd nm graph a (None,`Cons((b,ab_data),`Nil))
      | Some (data, adjs) ->
         let nm1,nm2 = Name.fork nm in
         let adjs = `Art(name_adjs nm2 adjs) in
         NodeMap.nadd nm1 graph a (data,`Cons((b,ab_data),adjs))

    let rem_node graph nm a =
      failwith "TODO"

    let rem_edge graph nm a b =
      failwith "TODO"

  end

*)
