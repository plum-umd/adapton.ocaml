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
    module NodeVal = AdaptonTypes.Tuple2(NodeData)(AdjSt.List.Data)
                                     
    module NodeMap = Trie.Map.Make(Trie.Useful(NodeId))
                                  (Trie.Useful(NodeVal))
                                  (Name)(ArtLib)


end

