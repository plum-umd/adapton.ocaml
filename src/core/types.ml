(** Convenience modules for built-in and other useful types. *)

module Int =
struct
    type t = int
    let hash = Hashtbl.seeded_hash
    let compare = compare
    let equal = (==)
    let show = string_of_int
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let sanitize x = x
end

module List(A : Data.S) =
struct
  type t = A.t list
  let rec hash seed = function
    | a::l -> A.hash (hash seed l) a
    | nil  -> Hashtbl.seeded_hash seed nil
  let equal x x' =
    try (x == x') || (List.for_all2 A.equal x x')
    with Invalid_argument _ -> false
  let sanitize = List.map A.sanitize
  let show l =
    if List.length l > 0 then
      let elts = List.fold_right (fun elt a -> (A.show elt)^", "^a) l "" in
      "["^(String.sub elts 0 ((String.length elts)-2))^"]"
    else "[]"
  let pp fmt s = Format.fprintf fmt "%s" (show s)
  let compare : t -> t -> int = compare
end

module IntList = List(Int)

module Char = struct
    type t = char
    let hash = Hashtbl.seeded_hash
    let equal = (==)
    let compare = compare
    let show = Char.escaped
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let sanitize x = x
end

module String = struct
    type t = string
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let show x = x
    let sanitize x = x
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare = String.compare
end

module Float = struct
    type t = float
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let show = string_of_float
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let sanitize x = x
    let compare = compare
end

module Bool = struct
    type t = bool
    let hash = Hashtbl.seeded_hash
    let equal = (==)
    let show = string_of_bool
    let compare = compare
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let sanitize x = x
end

module Int32 = struct
    type t = int32
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let sanitize x = x
    let show = Int32.to_string
    let compare = Int32.compare
    let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Int64 = struct
    type t = int64
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let sanitize x = x
    let compare = Int64.compare
    let show = Int64.to_string
    let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Nativeint = struct
    type t = nativeint
    let hash = Hashtbl.seeded_hash
    let show = Nativeint.to_string
    let compare = Nativeint.compare
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let equal = (=)
    let sanitize x = x      
end

module Option (A : Data.S) = struct
    type t = A.t option
    let hash seed = function
        | Some a -> A.hash (Hashtbl.seeded_hash seed `Some) a
        | None -> Hashtbl.seeded_hash seed `None
    let compare t t' = compare (hash 42 t) (hash 42 t')
    let equal x x' = x == x' || match x, x' with
        | Some a, Some a' -> A.equal a a'
        | _ -> false
    let sanitize x = match x with
      | None -> None
      | Some x -> Some (A.sanitize x)
    let show x = match x with
      | None -> "None"
      | Some x -> "Some("^(A.show x)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Tuple2(A : Data.S)(B : Data.S) = struct
    type t = A.t * B.t
    let hash seed ( a, b ) = B.hash (A.hash seed a) b
    let equal ( a, b as x ) ( a', b' as x' ) = x == x' || A.equal a a' && B.equal b b'
    let sanitize (a, b) = (A.sanitize a, B.sanitize b)
    let show (a,b) = "("^(A.show a)^","^(B.show b)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Tuple3 (A : Data.S) (B : Data.S) (C : Data.S) = struct
    type t = A.t * B.t * C.t
    let hash seed ( a, b, c ) = C.hash (B.hash (A.hash seed a) b) c
    let equal ( a, b, c as x ) ( a', b', c' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c'
    let sanitize (a, b, c) = (A.sanitize a, B.sanitize b, C.sanitize c)
    let show (a,b,c) = "("^(A.show a)^","^(B.show b)^","^(C.show c)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Tuple4 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) = struct
    type t = A.t * B.t * C.t * D.t
    let hash seed ( a, b, c, d ) = D.hash (C.hash (B.hash (A.hash seed a) b) c) d
    let equal ( a, b, c, d as x ) ( a', b', c', d' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d'
    let sanitize (a, b, c, d) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d)
    let show (a,b,c,d) = "("^(A.show a)^","^(B.show b)^","^(C.show c)^","^(D.show d)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Tuple5 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t
    let hash seed ( a, b, c, d, e ) = E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e
    let equal ( a, b, c, d, e as x ) ( a', b', c', d', e' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e'
    let sanitize (a, b, c, d, e) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e)
    let show (a,b,c,d,e) = "("^(A.show a)^","^(B.show b)^","^(C.show c)^","^(D.show d)^","^(E.show e)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Tuple6 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t
    let hash seed ( a, b, c, d, e, f ) = F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f
    let equal ( a, b, c, d, e, f as x ) ( a', b', c', d', e', f' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e' && F.equal f f'
    let sanitize (a, b, c, d, e, f) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f)
    let show (a,b,c,d,e,f) = "("^(A.show a)^","^(B.show b)^","^(C.show c)^","^(D.show d)^","^(E.show e)^","^(F.show f)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Tuple7 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) (G: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t
    let hash seed ( a, b, c, d, e, f, g ) = G.hash (F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f) g
    let equal ( a, b, c, d, e, f, g as x ) ( a', b', c', d', e', f', g' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e' && F.equal f f' && G.equal g g'
    let sanitize (a, b, c, d, e, f, g) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f, G.sanitize g)
    let show (a,b,c,d,e,f,g) = "("^(A.show a)^","^(B.show b)^","^(C.show c)^","^(D.show d)^","^(E.show e)^","^(F.show f)^","^(G.show g)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Tuple8 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) (G: Data.S) (H: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t
    let hash seed ( a, b, c, d, e, f, g, h ) =
      H.hash (G.hash (F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f) g) h
    let equal ( a, b, c, d, e, f, g, h as x ) ( a', b', c', d', e', f', g', h' as x' ) =
      x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e' && F.equal f f' && G.equal g g' && H.equal h h'
    let sanitize (a, b, c, d, e, f, g, h) =
      (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f, G.sanitize g, H.sanitize h)
    let show (a,b,c,d,e,f,g,h) =
      "("^(A.show a)^","^(B.show b)^","^(C.show c)^","^(D.show d)^","^(E.show e)^","^(F.show f)^","^(G.show g)^","^(H.show h)^")"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare t t' = compare (hash 42 t) (hash 42 t')
end

module Unit = struct
    type t = unit
    let hash seed () = seed
    let equal = (==)
    let show _ = "()"
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let compare _ _ = 0
    let sanitize () = ()
end

(** Counter. *)
module Counter = struct
    type t = int ref
    let make x : t = ref x
    let next c = let x = !c in incr c; x
end

(** Random number stream. *)
module Seeds = struct
    type t = Seeds of int * t Lazy.t
    let make ?seeds:seeds_opt () =
(*
      assert false (* XXX -- do not reset my seed! *)
*)
        let rng = match seeds_opt with
            | Some seeds -> Random.State.make seeds
            | None -> Random.State.make_self_init ()
        in
        let rec seeds () = Seeds ( Random.State.bits rng, Lazy.from_fun seeds ) in
        seeds ()

    let pop ( Seeds ( s, lazy seeds ) ) = ( s, seeds )
    let hash seed ( Seeds ( s, _ ) ) = Hashtbl.seeded_hash seed s
    let equal = (==)
    let sanitize x = x
    let string (Seeds(x,_)) = (Printf.sprintf "Seeds(%d,?)" x)
end

(** Infer and make Function modules. *)
let makeFunction (type a) (type b) () : (module Data.S with type t = a -> b) =
    (module struct
        type t = a -> b
        let equal = (==)
        let hash = Hashtbl.seeded_hash
        let compare = compare
        let show x = "Types.ml: makeFunction: no string"
        let pp fmt s = Format.fprintf fmt "%s" (show s)
        let sanitize x = x (* Lookout! XXX: May not actually sanitize!! *)
    end)

module Sum2 (A : Data.S) (B : Data.S) = struct
  type t = InL of A.t | InR of B.t
  let rec equal lhs rhs =
    match (lhs, rhs) with
    | (InL lhs0,InL rhs0) -> A.equal lhs0 rhs0
    | (InR lhs0,InR rhs0) -> B.equal lhs0 rhs0
    | _ -> false
  let hash seed = function
    | InL a -> A.hash seed a
    | InR b -> B.hash seed b
  let sanitize = function
    | InL a -> InL (A.sanitize a)
    | InR b -> InR (B.sanitize b)
  let string = function
    | InL a -> Printf.sprintf "InL (%s)" (A.show a)
    | InR b -> Printf.sprintf "InR (%s)" (B.show b)
end
