module PrimList = List
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

  let hash : int -> sto -> int   = List.Data.hash
  let string : sto -> string     = List.Data.string
  let sanitize : sto -> sto      = List.Data.sanitize
  let equal : sto -> sto -> bool = List.Data.equal

end

module StoStringInt = AssocStore (Types.String)(Types.Int)
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
  | AWhile of aexpr * bexpr * cmd

let rec aeval s = function
  | Int n -> n
  | Plus (e1, e2) -> (aeval s e1) + (aeval s e2)
  | Minus (e1, e2) -> (aeval s e1) - (aeval s e2)
  | Times (e1, e2) -> (aeval s e1) * (aeval s e2)
  | Var x ->
     (match lookup s x with
      | Some i -> i
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
  | Assign of Name.t * string * aexpr
  | Seq of 'a art_cmd * 'a art_cmd
  | If of bexpr * 'a art_cmd * 'a art_cmd
  | While of Name.t * bexpr * 'a art_cmd
  | AWhile of (Name.t * aexpr) * bexpr * 'a art_cmd
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
                         let rec string x = match x with
                           | Skip -> "Skip"
                           | Assign(nm,x,a) -> Printf.sprintf "Assign(%s,%s,_)" (Name.string nm) x
                           | Seq(c1,c2) -> Printf.sprintf "Seq(%s,%s)" (string c1) (string c2)
                           | If(b,c1,c2) -> Printf.sprintf "If(_,%s,%s)" (string c1) (string c2)
                           | While(nm,b,c) -> Printf.sprintf "While(%s,_,%s)" (Name.string nm) (string c)
                           | AWhile((nm,a),b,c) -> Printf.sprintf "AWhile((%s,_),_,%s)" (Name.string nm) (string c)
                           | Art a -> Printf.sprintf "Art %s" (Cmd.Art.string a)
                           | Name(nm,c) -> Printf.sprintf "Name(%s,%s)" (Name.string nm) (string c)
                         let rec hash seed x = match x with
                             Skip as c -> Hashtbl.seeded_hash seed c
                           | Assign(nm, x,a) as c -> Hashtbl.seeded_hash (Name.hash seed nm) c
                           | Seq(c1,c2) -> hash (hash seed c1) c2
                           | If (b, c1, c2) -> hash (hash (Hashtbl.seeded_hash seed b) c1) c2
                           | While (nm, b, c) -> hash (Hashtbl.seeded_hash (Name.hash seed nm) b) c
                           | AWhile((nm,a),b,c) -> hash (Hashtbl.seeded_hash (Hashtbl.seeded_hash (Name.hash seed nm) a) b) c
                           | Art a -> Cmd.Art.hash seed a
                           | Name (nm, c) -> hash (Name.hash seed nm) c
                         let rec equal c1 c2 = match (c1, c2) with
                           | Skip, Skip -> true
                           | Assign(nm1,x,a), Assign(nm2,y,b) -> Name.equal nm1 nm2 && x = y && a = b
                           | Seq(a1, b1), Seq(a2, b2) -> equal a1 a2 && equal b1 b2
                           | If (a, b, c), If (d, e, f) -> a = d && equal b e && equal c f
                           | While(nm1, b, c), While(nm2, d, e) -> Name.equal nm1 nm2 && b = d && equal c e
                           | AWhile((nm1,a),b,c), AWhile((nm2,d),e,f) -> a = d && Name.equal nm1 nm2 && b = e && equal c f
                           | Name(n, a), Name(m, b) -> Name.equal n m && equal a b
                           | Art a, Art b -> Cmd.Art.equal a b
                           | _, _ -> false
                         let rec sanitize x = x
                       end
                       (* Apply the library's functor: *)
                       module Art = ArtLib.MakeArt(Name)(Data)
                     end

(* Deadcode: *)
let cmd_mfn =
  Cmd.Art.mk_mfn
    (Name.gensym "cmd")
    (module Cmd.Data)
    (fun _ c -> c)

let name_of_int_list nm ints = 
  PrimList.fold_left
    (fun nm i ->
     Name.pair (Name.gensym (string_of_int i)) nm)
    nm ints
  
let rec ceval =
  let mfn =
    List.Art.mk_mfn
      (Name.gensym "ceval")
      (module Types.Tuple4(Name)(Types.IntList)(List.Data)(Cmd.Data))
      (fun mfn (outernm, coord, s, cmd) ->
       let ceval outernm coord s c = mfn.List.Art.mfn_data (outernm, coord,s,c) in
       let ceval_same_loop s c = ceval outernm coord s c in
       match cmd with
       | Skip -> s
       | Assign (assign_nm, x, a) ->
          let nm = name_of_int_list assign_nm coord in
	  let i = aeval s a in
          Printf.printf " | ext | (%s,%s) | (%s,%d) \n%!"
                        (Name.string assign_nm)
                        (Types.IntList.string coord) x i ;
          ext nm s x i

       | Seq (c0, c1) -> ceval_same_loop (ceval_same_loop s c0) c1

       | If (b, c0, c1) ->
          (match beval s b with
             true -> ceval_same_loop s c0
           | false -> ceval_same_loop s c1)


       | (While (nm, b, c)) as w ->
          if outernm = nm then (* same loop *)
            let default_idx::coord_suff = coord in
            let coord = default_idx+1::coord_suff in
            ceval nm coord s (If (b, Seq(c, w), Skip))

          else (* entering an inner loop for the first time. *)
            let coord = 0 :: coord in
            ceval nm coord s (If (b, Seq(c, w), Skip))

       | (AWhile ((nm,a), b, c)) as w ->
          (* Compute a new coordinate *)
          let a_idx = aeval s a in
          let coord = match coord with
            | [] -> [a_idx]
            | _::more when Name.equal outernm nm -> a_idx::more
            | outer_coord -> a_idx :: outer_coord
          in
          ceval nm coord s (If (b, Seq(c, w), Skip))

       | Art a ->
          ceval_same_loop s (Cmd.Art.force a)

       | Name(nm, cmd) ->
          (* Note: nm is not unique enough. *)
          (* Perhaps use the nm at the head of the store? *)
          let nm = (name_of_int_list nm coord) in
          let art = mfn.mfn_nart nm (outernm,coord,s,cmd) in
          Printf.printf " | memo | %s | | %s | %s | %s | %s \n"
                        (Name.string nm)
                        (Name.string outernm)
                        (Types.IntList.string coord)
                        (List.Data.string s)
                        (Cmd.Data.string cmd) ;
          List.Art.force art
      )
  in
  fun outernm coord cmd s -> mfn.mfn_data (outernm, coord, cmd, s)

let rec seq : cmd list -> cmd = fun cs ->
  match cs with
  | [] -> Skip
  | c::cs -> Seq (c, seq cs)

let fact : cmd =
  seq[ Assign ("z", Int 0);
       Assign ("n", Int 5);
       Assign ("f", Int 1);
       While (Leq (Int 1, Var "n"),
	      seq [Assign ("f", Times (Var "n", Var "f"));
		   Assign ("n", Minus (Var "n", Int 1))])]

let rec leftmost_assign : Cmd.Data.t -> Cmd.Art.t option = function
  | Name (nm, Art (a)) ->
     (match (Cmd.Art.force a) with
     | Skip -> None
     | Assign _ -> Some a
     | Seq (c1, c2) -> leftmost_assign c1
     | If (b, c1, c2) -> leftmost_assign c1
     | While (a, b, c) -> leftmost_assign c)

let replace_leftmost : Cmd.Data.t -> Cmd.Data.t -> unit =
  fun cmd cmd0 ->
  let Some a = leftmost_assign cmd in
  Cmd.Art.set a cmd0


let rec annotate : cmd -> Cmd.Data.t =
  fun c ->
  let recur (c:cmd) = match c with
    | Skip -> Skip
    | Assign (x, a) -> Assign (Name.nondet (), x, a)
    | Seq (c1, c2) ->
       Seq (annotate c1, annotate c2)
    | If (b, c1, c2) ->
       If (b, annotate c1, annotate c2)
    | While (b, c) ->
       While (Name.nondet (), b, annotate c)
  in
  Name (Name.nondet (),
        Art (Cmd.Art.cell (*cmd_mfn.mfn_nart*)
               (Name.nondet ()) (recur c)))

(*
  | Art of 'a
  | Name of Name.t * 'a art_cmd
 *)

(*
let main =
  let p = annotate fact in
  let s = ceval `Nil p in
  print_string (List.Data.string s);
  replace_leftmost p (Assign ("x", Int 6));
  let s1 = ceval `Nil p in
  print_string (List.Data.string s1)
 *)

let test_cmd_mutation cmd storein mutator =
  let root_loop = Name.nondet () in
  let (storeout, stats1) =
    AdaptonStatistics.measure (
        (fun () -> ceval root_loop [] storein cmd)
      )
  in
  Printf.printf "sto1=%s\n" (List.Data.string storeout) ;
  mutator cmd ;
  let (storeout, stats2) =
    AdaptonStatistics.measure (
        (fun () -> ceval root_loop [] storein cmd)
      )
  in
  Printf.printf "sto2=%s\n" (List.Data.string storeout) ;
  (stats1, stats2)

let stats_print msg stats =
  Printf.printf "%s: dirty:%d, clean:%d, create: %d, evaluate: %d\n%!"
                msg
                stats.AdaptonStatistics.dirty
                stats.AdaptonStatistics.clean
                stats.AdaptonStatistics.create
                stats.AdaptonStatistics.evaluate

let main () =
  let stats1, stats2 = test_cmd_mutation
			 (annotate fact)
			 `Nil
			 (fun p -> replace_leftmost p (Assign (Name.nondet(), "z", Int 6)))
  in
  stats_print "run1" stats1 ;
  stats_print "run2" stats2 ;
  ()


let _ =
main ()
