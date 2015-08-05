(* After building and installing Adapton, we can reference the OPAM
 * package by the name "adapton" in our ocamlbuild _tags file, as well
 * as the top-level module named `Adapton`, as below. The core modules
 * that the top-level `Adapton` exposes are:
 *
 *   core abstractions:   Art.S, Data.S, Name.S
 *   Art.S factory:       MakeArt.Of
 *   common Data.S impls: Types.{Int, Float, Int32, Int64, Nativeint,
 *                               Bool, Char, String, Unit,
 *                               List, Option, Tuple{2..8}}
 *   run-time stats:      Statistics
*)
open Adapton

(* The core abstraction that Adapton provides to build incremental
 * computations is an Articulation Point (art for short). Arts are
 * locations in our program where Adapton redirects control through
 * its internal memotables. Arts are either atomic cells that can be
 * mutated (like OCaml's ref) or the result of some memoized
 * function call.
 *
 * To get a hold of an art that wraps some type t, we need make a
 * module D with the module type `Data.S`, where D.t = t.
 *
 * module type Art.S =
 * sig
 *   include Data.S   (* see below for Data.S contents *)
 *   type name
 *   type data
 *   
 *   type 'a mfn = ... (* makes memoized functions of type *)
 *   val  mk_mfn : ... (* 'a mfn, see below for details    *)
 *   
 *   (* creates mutable arts with some name and wrapped data *)
 *   val cell  : name -> data -> t
 *
 *   (* mutates the value of an art
 *    * (will blow up if called on a mfn result!) *)
 *   val set   : t -> data -> unit
 *
 *   (* unwrap the data under the articulation point, evaluating
 *    * any mfn calls that had yet to be forced. *)
 *   val force : t -> data
 *
 * end
 *
 * `AInt` below is an art that holds an int. We create arts with the
 * functor `MakeArt.Of`, which expects two arguments. The first is a
 * module of module type `Name.S`: the names we give to the resulting
 * arts. The `Name` module itself is a simple Name.S impl. The
 * `Data.S` module impl for the type we are wrapping. The `Types`
 * module provides common `Data.S` instances for some OCaml primitive
 * types; we use `Types.Int` below.
 *
 *)
module AInt = MakeArt.Of(Name)(Types.Int)

(* `SSVal` is a module of type `Data.S` that represents the entries
 * of a spreadsheet. We can have either atomic entries (which hold
 * a single int behind an art: an `AInt.t`) or products of other
 * `SSVal`s.
 * 
 * module type Data.S =
 * sig
 *   type t
 *   val equal    : t -> t -> bool
 *   val compare  : t -> t -> int
 *   val pp       : Format.formatter -> t -> unit
 *   val show     : t -> string
 *   val hash     : int -> t -> int
 *   val sanitize : t -> t
 * end
 *
 * The only non-obvious member of a `Data.S` impl is sanitize, which
 * just makes a structural copy (call sanitize all the way down,
 * including arts, if any). If we use ppx_deriving, we can generate
 * structural versions of equal, compare, and pp/show with
 * [@@deriving eq, ord, show].
 * 
 * TODO: ppx extensions for structural hash/copy, so fully structural
 *       `Data.S` modules can be generated automatically.
 *)
module SSVal (* : Data.S with type t as below*) =
struct
  type t = Atom of AInt.t
         | Prod of t * t
  [@@deriving eq, ord, show]
  let rec hash seed = function
    | Atom ai      -> AInt.hash seed ai
    | Prod (t, t') -> hash (hash seed t) t'
  let rec sanitize = function
    | Atom ai      -> Atom (AInt.sanitize ai)
    | Prod (t, t') -> Prod (sanitize t, sanitize t')
end
open SSVal

(* The function `eval` defined below evaluates a spreadsheet entry.
 *  There's a lot going on here, so we'll walk through it.
 *
 * TODO: Investigate ways to simplify working with arts:
 *       - an art monad, with a memoized bind
 *       - ppx_implicit to avoid passing modules around
 *
 * `eval`'s type is `SSVal.t -> AInt.t`, so it operates on data that
 * contains int arts (of type AInt.t in the Atom constructor) and
 * returns an AInt.t that contains the entry's value. Incremental
 * computations built with Adapton often have this form so they
 * can (fairly) easily be chained together.
 * 
 * We create a single memoized function `memo` with the `mk_mfn`
 * function of the _return type's_ art module, in this case `AInt`.
 *
 * AInt.mk_mfn : Name.t ->
 *               (module D : Data.S) ->
 *               (D.t AInt.mfn -> D.t -> AInt.t) ->
 *               D.t AInt.mfn
 *
 * The first argument is a name for the memofunction, which Adapton
 * uses to disambiguate memotables.
 *
 * TODO: I think this is a lie, engine.ml seems to gensym names for
 * the memotables. I'm failing to come up with a good use case for
 * sharing a memotable between two functions. Why is this here? Is
 * it just for easier reading of the runtime statistics?
 *
 * The second argument is the `Data.S` impl of the input to the
 * function, as a first-class module.
 *
 * The third is a generator for the function we're trying to write.
 * The only difference between the generator and the function itself
 * is that if we need to recur (as in our `eval`), we need to recur
 * through the first argument of the generator rather than the top-
 * level `eval`. Adapton offers three different ways for us to recur
 * in some `D.t AInt.mfn` record, with the following type:
 *
 *   { mfn_data : D.t -> AInt.data       (* pure recursion, where *)
 *                                       (* `AInt.data` = int     *)
 *   ; mfn_art  : D.t -> AInt.t          (* structurally memoized *)
 *   ; mfn_nart : AInt.name -> D.t -> t  (* nominally memoized,   *)
 *   }                                   (* where `AInt.name` is  *
 *                                        * in then `Name.S`      *
 *                                        * module we passed to   *
 *                                        * `MakeArt`.            *)
 * 
 * In `eval` we only need structural recursion, so we alias
 * `memo.AInt.mfn_art` as `eval` in the recursive case.
 * 
 * Finally, `AInt.mk_mfn ... (module D) ...` returns a `D.t AInt.mfn`
 * which can be used in any user code.
 *)
let eval : SSVal.t -> AInt.t =
  let memo =
    AInt.mk_mfn
      (Name.of_string "toplevel#eval#memo") (* mfn name *)
      (module SSVal) (* input type's Data.S impl *)
      (fun memo -> function (* generator, so we recur through memo *)
         | Atom ai      -> AInt.force ai
         | Prod (t, t') ->
           let eval = memo.AInt.mfn_art in
           AInt.force (eval t) * AInt.force (eval t'))
  in
  memo.AInt.mfn_art


let mutate : SSVal.t -> int -> unit =
  fun ssv i' -> match ssv with
    | Atom ai -> print_endline "Mutating one of the entries!" ;
                 AInt.set ai i'
    | _       -> failwith "Refusing to set a Prod"

let print : AInt.t -> unit =
  fun ai ->
    Printf.printf "The spreadsheet entry's value is %i.\n%!"
      (AInt.force ai)


(* For this simple example we don't need to disambiguate AInt.t arts
 * based on anything but their structural content, so we just turn
 * each int into a string to name them.
 * 
 * TODO: Simple example using names. I think that examples where the
 *       input data is already articulated are unenlightening, since
 *       the reader may not know when/where/why to place arts in the
 *       input data. How about showing a function that will generate
 *       email messages to people based on unreliably entered info?
 *       Use the SSN as the name for call to `make_msg` given some
 *       `person` record containing SSN and some other identifying
 *       data (surname, address) that may have case/spelling/format
 *       inconsistency that is normalized by `make_msg`? Anything
 *       more compelling?
 *
 *       Once I can show the more enlightening example, showing
 *       a call the inc-imp's evaluator (even eliding most of the
 *       details of the eval impl) would be illuminating, since
 *       I could show the swap pattern, how to articulate input
 *       data based on domain knowledge (source positions), etc.
 *)
let nm   (i : int) : Name.t = Name.of_string (string_of_int i)
let cell (i : int) : AInt.t = AInt.cell (nm i) i

let test eval =
  print_endline "Running eval test." ;
  let ssv2    = Atom (cell 2)
  and ssv3    = Atom (cell 3)
  and ssv4    = Atom (cell 4)  in
  let ssvP23  = Prod (ssv2,   ssv3) in
  let ssvPP4  = Prod (ssv4, ssvP23) in
  let evalArt = eval ssvPP4 in
  print evalArt ;
  mutate ssv4 5 ;
  print evalArt

let _ = test eval
(* => prints to stdout:
Running eval test.
The spreadsheet entry's value is 24.
Mutating one of the entries!
The spreadsheet entry's value is 30.
 *)

(* `print_eval` below has the same behavior as `eval`, but prints
 * something on every call so we can see where Adapton is saving
 * us from strenuous spreadsheet multiplications.
 * 
 * WARNING: `print_eval` below is not from-scratch consistent!
 * Adapton guarantees from-scratch consistency (the same program
 * behavior as if you weren't memoizing or using cells) if and only
 * inside a memoized function you are performing purely functional
 * operations on purely functional data (no mutation or side
 * effects).
 *
 * Also, never call Art.set inside a memoized function. The behavior
 * of mfns that break this rule is undefined: dragons may fly out of
 * your screen if you do.
 *)
let print_eval : SSVal.t -> AInt.t =
  let memo =
    AInt.mk_mfn
      (Name.of_string "toplevel#print_eval#memo")
      (module SSVal)
      (fun memo -> function
         | Atom ai      ->
           let i = AInt.force ai in
           Printf.printf "  eval %i\n%!" i ;
           i
         | Prod (t, t') ->
           let eval = memo.AInt.mfn_art in
           let i  = AInt.force (eval t)
           and i' = AInt.force (eval t') in
           Printf.printf "  eval %i * %i\n%!" i i' ;
           i*i')
  in
  memo.AInt.mfn_art

let _ = test print_eval
(* => prints to stdout:
Running eval test.
eval 4
eval 2
eval 3
eval 2 * 3
eval 4 * 6
The spreadsheet entry's value is 24.
Mutating one of the entries!
eval 5
eval 5 * 6
The spreadsheet entry's value is 30.
 *)

(* As we can see above, we only perform 2 calls to `eval` when we
 * force the result after mutation, rather to the original 6.
 * Woohoo!
 * 
 * Go forth and articulate!
 *)
