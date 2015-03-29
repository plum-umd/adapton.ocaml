open Adapton_core
open Primitives
open GrifolaType

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
        (Name.gensym "Graph.name_cons")
        (module AdjSt.List.Data)
        (fun _ adj_list -> adj_list))
        .AdjSt.List.Art
        .mfn_nart

    let add_edge graph nm a b ab_data =
      match NodeMap.find graph a with
      | None ->
         NodeMap.nadd nm graph a (None,`Cons((b,ab_data),`Nil))
      | Some (data, adjs) ->
         let nm1,nm2 = Name.fork nm in
         let adjs = `Art(name_adjs nm2 adjs) in
         NodeMap.nadd nm1 graph a (data,`Cons((b,ab_data),adjs))

  end

