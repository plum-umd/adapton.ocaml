(* use Adapton *)
open Adapton_lib

(* setup adapton types *)
module N = Adapton.Name
module T = Adapton.Types
module Point = T.Tuple2(T.Int)(T.Int)

(* setup adapton list for no reason *)
open Adapton.Structures.SpreadTree
module PointST = MakeSpreadTree(Adapton.NameLib)(N)(Point)
module PointSeq = MakeSeq(PointST)

(* setup adapton for simple temp sim *)
module AInt = Adapton.NameLib.MakeArt(N)(T.Int)

(* define streams *)
