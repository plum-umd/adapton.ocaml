(** Memoization helper module to memoize incremental thunks. *)

open Primitives

(** Input module type of memoization functor {!MemoNK.Make}. *)
module type MemoNKType = sig
    type data
    type t
(*
  val memo : (module Hashtbl.SeededHashedType with type t = 'a) ->
  (('a -> t as 'f) -> 'a -> data) -> 'f
*)

    (* Nominal feature: memo_keyed *)
    val memo_keyed :
      (module ResultType with type t = 'arg) ->
      ?symbol:Symbol.t ->
      ((Key.t -> 'arg -> t) -> 'arg -> data) -> (Key.t -> 'arg -> t)

		(*
		(* Modified for Yit's proposed changes. *)
		val memo_keyed' :
      (module ResultType with type t = 'arg) ->
      ?symbol:Symbol.t ->
      ((Key.t * 'arg -> t) -> (Key.t * 'arg) -> data) -> (Key.t * 'arg -> t)
		*)
end

(** Output module type of memoization functor {!MemoNK.Make}. *)
module type S = sig
    type data
    type t
    val memo_keyed :
      (module ResultType with type t = 'arg) ->
      ?symbol:Symbol.t ->
      ((Key.t -> 'arg -> t) -> 'arg -> data) -> (Key.t -> 'arg -> t)
    val memo_keyed2 :
      (module ResultType with type t = 'a) ->
      (module ResultType with type t = 'b) ->
      ?symbol:Symbol.t ->
      ((Key.t -> 'a -> 'b -> t as 'f) -> 'a -> 'b -> data) -> 'f
    val memo_keyed3 :
      (module ResultType with type t = 'a) ->
      (module ResultType with type t = 'b) ->
      (module ResultType with type t = 'c) ->
      ?symbol:Symbol.t ->
      ((Key.t -> 'a -> 'b -> 'c -> t as 'f) -> 'a -> 'b -> 'c -> data) -> 'f
end

(** Functor to make memoizing constructor of arity of 2 or greater from a memoizing constructor of arity 1. *)
module Make (M : MemoNKType) = struct
    (** Create memoizing constructor of arity 1. *)
    let memo_keyed = M.memo_keyed

    (** Create memoizing constructor of arity 2. *)
    let memo_keyed2
        (type a) (module A : ResultType with type t = a)
        (type b) (module B : ResultType with type t = b)
        ?symbol:(symbol=Symbol.unknown)
        f =
      let memo_keyed2 = M.memo_keyed ~symbol:symbol (
        module struct
          type t = A.t * B.t
          let hash seed ( a, b ) = B.hash (A.hash seed a) b
          let equal ( a, b as x ) ( a', b' as x' ) = x == x' || A.equal a a' && B.equal b b'
          let string ( x , y ) = Printf.sprintf "%s %s" (A.string x) (B.string y)
          let sanitize ( x, y ) = ( A.sanitize x , B.sanitize y )
        end)
        (fun memo ( a, b ) -> f (fun k a b -> memo k ( a, b )) a b)
      in
      fun k a b -> memo_keyed2 k ( a, b )

    (** Create memoizing constructor of arity 3. *)
    let memo_keyed3
        (type a) (module A : ResultType with type t = a)
        (type b) (module B : ResultType with type t = b)
        (type c) (module C : ResultType with type t = c)
        ?symbol:(symbol=Symbol.unknown)
        f =
      let memo_keyed3 = M.memo_keyed ~symbol:symbol (
        module struct
          type t = A.t * B.t * C.t
          let hash seed ( a, b, c ) = C.hash (B.hash (A.hash seed a) b) c
          let equal ( a, b, c as x ) ( a', b', c' as x' ) =
            x == x' || A.equal a a' && B.equal b b' && C.equal c c'
          let string ( x , y , z ) = Printf.sprintf "%s %s %s" (A.string x) (B.string y) (C.string z)
          let sanitize ( x, y, z ) = ( A.sanitize x , B.sanitize y, C.sanitize z )
        end)
        (fun memo ( a, b, c ) -> f (fun k a b c -> memo k ( a, b, c )) a b c)
      in
      fun k a b c -> memo_keyed3 k ( a, b, c )
end
