include Adapton_lib

open Adapton_structures
open Adapton_core
open Primitives
open GrifolaType

module Types = AdaptonTypes
module Statistics = AdaptonStatistics
module ArtLib (* : ArtLibType *) = Grifola.Default.ArtLib
module Name : NameType = Key

module type Store = sig
    type sto
    type t = sto
    type a
    type b
    val mt : sto
    val lookup : sto -> a -> b option
    val ext : sto -> a -> b -> sto
    val hash : int -> sto -> int
    val string : sto -> string
    val sanitize : sto -> sto
    val equal : sto -> sto -> bool
  end

module AssocStore (A:DatType)(B:DatType) = struct
  type a = A.t
  type b = B.t

  module St = SpreadTree.MakeSpreadTree(ArtLib)(Name)
                                       (Types.Tuple2(A)(B))

  module List = St.List
  type sto = List.Data.t
  type t = sto

  let mt = []

  let rec lookup : sto -> 'a -> 'b option =
    fun s x -> match s with
                 `Cons ((y, b), s) when (y = x) -> Some b
               | `Cons (_, s) -> lookup s x
               | `Nil -> None
               | `Name(_, s) -> lookup s x
               | `Art(a) -> lookup (List.Art.force a) x

  let ext : Name.t -> sto -> 'a -> 'b -> sto =
    let list_mfn =
      List.Art.mk_mfn (Name.gensym "sto") (module List.Data) (fun _ l -> l)
    in
    fun nm s x v ->
    let nm1, nm2 = Name.fork nm in
    `Name(nm1, `Art( list_mfn.mfn_nart nm2 (`Cons ((x, v), s))))

  (* FIXME: degenerate hash *)
  let hash : int -> sto -> int =
    fun seed s ->
    0

  (* FIXME: *)
  let string : sto -> string =
    fun s ->
    failwith "Not implemented"

  (* FIXME: no op *)
  let sanitize : sto -> sto =
    fun s -> s

  let equal : sto -> sto -> bool =
    fun s1 s2 -> s1 = s2

end

module StoStringInt = AssocStore (Types.String)(Types.Tuple2(Types.Int)(Types.Int))
open StoStringInt

type store = sto

type aexpr =
  | Int of int
  | Plus of aexpr * aexpr
  | Minus of aexpr * aexpr
  | Times of aexpr * aexpr
  | Var of string

type bexpr =
  | True
  | False
  | Not of bexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Eq of aexpr * aexpr
  | Leq of aexpr * aexpr

type cmd =
  | Skip
  | Assign of string * aexpr
  | Seq of cmd * cmd
  | If of bexpr * cmd * cmd
  | While of bexpr * cmd

let rec aeval s = function
  | Int n -> n
  | Plus (e1, e2) -> (aeval s e1) + (aeval s e2)
  | Minus (e1, e2) -> (aeval s e1) - (aeval s e2)
  | Times (e1, e2) -> (aeval s e1) * (aeval s e2)
  | Var x ->
     (match lookup s x with
      | Some (i, j) -> i
      | None -> failwith "Unset variable")

let rec beval s = function
  | True -> true
  | False -> false
  | Not b -> not (beval s b)
  | And (b1, b2) -> (beval s b1) && (beval s b2)
  | Or (b1, b2) -> (beval s b1) || (beval s b2)
  | Leq (a1, a2) -> (aeval s a1) <= (aeval s a2)
  | Eq (a1, a2) -> (aeval s a1) = (aeval s a2)


type 'a art_cmd =
  | Skip
  | Assign of string * aexpr
  | Seq of 'a art_cmd * 'a art_cmd
  | If of bexpr * 'a art_cmd * 'a art_cmd
  | While of bexpr * 'a art_cmd
  (* Boilerplate cases: *)
  | Art of 'a
  | Name of Name.t * 'a art_cmd

module rec Cmd
           : sig
               module Data : DatType
               module Art : ArtType
             end
           with type Data.t = Cmd.Art.t art_cmd
            and type Art.Data.t = Cmd.Art.t art_cmd
            and type Art.Name.t = Name.t
                                  = struct
                       module Data = struct
                         type t = Cmd.Art.t art_cmd
                         let rec string x = failwith "todo"
                         let rec hash seed x = failwith "todo"
                         let rec equal xs ys = failwith "todo"
                         let rec sanitize x = failwith "todo"
                       end
                       (* Apply the library's functor: *)
                       module Art = ArtLib.MakeArt(Name)(Data)
                     end

let cmd_mfn =
  Cmd.Art.mk_mfn
    (Name.gensym "cmd")
    (module Cmd.Data)
    (fun _ c -> c)

let rec ceval cmd s =
  (* next step is to use mk_mfn *)

  let mfn =
    List.Art.mk_mfn
      (Name.gensym "ceval")
      (module Types.Tuple2(List.Data)(Cmd.Data))
      (fun mfn (s, cmd) ->
       let ceval s c = mfn.List.Art.mfn_data (s,c) in
       match cmd with
       | Skip -> s
       | Assign (x, a) ->
          let cnt = match lookup s x with
	    | None -> 0
	    | Some (_, count) -> count + 1
          in
          let nm = Name.pair (Name.gensym x)
                             (Name.gensym (string_of_int cnt))
          in
	  let i = aeval s a in
          ext nm s x (i, cnt)

       | Seq (c0, c1) -> ceval (ceval s c0) c1

       | If (b, c0, c1) ->
          (match beval s b with
             true -> ceval s c0
           | false -> ceval s c1)
       | (While (b, c)) as w -> ceval s (If (b, Seq(c, w), Skip))

       | Art a ->
          ceval s (Cmd.Art.force a)

       | Name(nm, cmd) ->
          List.Art.force (mfn.mfn_nart nm (s,cmd))
      )
  in
  mfn.mfn_data (cmd, s)

let rec annotate : cmd -> Cmd.Data.t =
  fun c ->
  let recur (c:cmd) = match c with
    | Skip -> Skip
    | Assign (x, a) -> Assign (x, a)
    | Seq (c1, c2) ->
       Seq (annotate c1, annotate c2)
    | If (b, c1, c2) ->
       If (b, annotate c1, annotate c2)
    | While (b, c) ->
       While (b, annotate c)
  in
  Name (Name.nondet (),
        Art (cmd_mfn.mfn_nart (Name.nondet ()) (recur c)))

(*
  | Art of 'a
  | Name of Name.t * 'a art_cmd
 *)

let test_cmd_mutation cmd storein mutator =
  let (storeout, stats1) =
    AdaptonStatistics.measure (
        (fun () -> ceval storein cmd)
      )
  in
  Printf.printf "sto1=%s\n" (List.Data.string storeout) ;
  mutator cmd ;
  let (storeout, stats2) =
    AdaptonStatistics.measure (
        (fun () -> ceval storein cmd)
      )
  in
  Printf.printf "sto2=%s\n" (List.Data.string storeout) ;
  (stats1, stats2)

let stats_print msg stats =
  Printf.printf "%s: create: %d, evaluate: %d\n%!"
                msg
                stats.AdaptonStatistics.create
                stats.AdaptonStatistics.evaluate

let main () =
  let stats1, stats2 = test_cmd_mutation
                         (failwith "todo")
                         (failwith "todo")
                         (fun cmd -> failwith "todo")
  in
  stats_print "run1" stats1 ;
  stats_print "run2" stats2 ;
  ()
