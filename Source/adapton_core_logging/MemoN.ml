(** Memoization helper module to memoize incremental thunks. *)

open Primitives

(** Input module type of memoization functor {!MemoN.Make}. *)
module type MemoNType = sig
    type data
    type t
    val memo : (module ResultType with type t = 'a) -> (('a -> t as 'f) -> 'a -> data) -> 'f
end

(** Output module type of memoization functor {!MemoN.Make}. *)
module type S = sig
    type data
    type t
    val memo :
        (module ResultType with type t = 'a)
        -> (('a -> t as 'f) -> 'a -> data) -> 'f
    val memo2 :
        (module ResultType with type t = 'a)
        -> (module ResultType with type t = 'b)
        -> (('a -> 'b -> t as 'f) -> 'a -> 'b -> data) -> 'f
    val memo3 :
        (module ResultType with type t = 'a)
        -> (module ResultType with type t = 'b)
        -> (module ResultType with type t = 'c)
        -> (('a -> 'b -> 'c -> t as 'f) -> 'a -> 'b -> 'c -> data) -> 'f
    val memo4 :
        (module ResultType with type t = 'a)
        -> (module ResultType with type t = 'b)
        -> (module ResultType with type t = 'c)
        -> (module ResultType with type t = 'd)
        -> (('a -> 'b -> 'c -> 'd -> t as 'f) -> 'a -> 'b -> 'c -> 'd -> data) -> 'f
end

(** Functor to make memoizing constructor of arity of 2 or greater from a memoizing constructor of arity 1. *)
module Make (M : MemoNType) = struct
    (** Create memoizing constructor of arity 1. *)
    let memo = M.memo

    (** Create memoizing constructor of arity 2. *)
    let memo2
            (type a) (module A : ResultType with type t = a)
            (type b) (module B : ResultType with type t = b)
            f =
        let memo = M.memo (module struct
            type t = A.t * B.t
            let hash seed ( a, b ) = B.hash (A.hash seed a) b
            let equal ( a, b as x ) ( a', b' as x' ) = x == x' || A.equal a a' && B.equal b b'
            let string ( x , y ) = Printf.sprintf "%s %s" (A.string x) (B.string y)
            let sanitize (a, b) = (A.sanitize a, B.sanitize b)
        end) (fun memo ( a, b ) -> f (fun a b -> memo ( a, b )) a b) in
        fun a b -> memo ( a, b )

    (** Create memoizing constructor of arity 3. *)
    let memo3
            (type a) (module A : ResultType with type t = a)
            (type b) (module B : ResultType with type t = b)
            (type c) (module C : ResultType with type t = c)
            f =
        let memo = M.memo (module struct
            type t = A.t * B.t * C.t
            let hash seed ( a, b, c ) = C.hash (B.hash (A.hash seed a) b) c
            let equal ( a, b, c as x ) ( a', b', c' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c'
            let string ( x , y , z ) = Printf.sprintf "%s %s %s" (A.string x) (B.string y) (C.string z)
            let sanitize (a, b, c) = (A.sanitize a, B.sanitize b, C.sanitize c)
        end) (fun memo ( a, b, c ) -> f (fun a b c -> memo ( a, b, c )) a b c) in
        fun a b c -> memo ( a, b, c )

    (** Create memoizing constructor of arity 4. *)
    let memo4
            (type a) (module A : ResultType with type t = a)
            (type b) (module B : ResultType with type t = b)
            (type c) (module C : ResultType with type t = c)
            (type d) (module D : ResultType with type t = d)
            f =
        let memo = M.memo (module struct
            type t = A.t * B.t * C.t * D.t
            let hash seed ( a, b, c, d ) = D.hash (C.hash (B.hash (A.hash seed a) b) c) d
            let equal ( a, b, c, d as x ) ( a', b', c', d' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d'
            let string ( x, y, z, w ) = Printf.sprintf "%s %s %s %s" (A.string x) (B.string y) (C.string z) (D.string w)
            let sanitize (a, b, c, d) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d)
        end) (fun memo ( a, b, c, d ) -> f (fun a b c d -> memo ( a, b, c, d )) a b c d) in
        fun a b c d -> memo ( a, b, c, d )
end
