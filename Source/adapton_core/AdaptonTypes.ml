(** Convenience modules for built-in and other useful types. *)

open Primitives

module Int = struct
    type t = int
    let hash = Hashtbl.seeded_hash
    let equal = (==)
    let string = string_of_int
    let sanitize x = x
end

module List (A : DatType) = struct
    type t = A.t list
    let rec hash seed = function
        | a::l -> A.hash (hash seed l) a
        | nil  -> Hashtbl.seeded_hash seed nil
    let equal x x' =
      try (x == x') || (List.for_all2 A.equal x x')
      with Invalid_argument _ -> false
    let sanitize = List.map A.sanitize
    let string l =
      if List.length l > 0 then
        let elts = List.fold_right (fun elt a -> (A.string elt)^", "^a) l "" in
        "["^(String.sub elts 0 ((String.length elts)-2))^"]"
      else "[]"
    let compare : t -> t -> int = compare
end

module IntList = List(Int)

module Char = struct
    type t = char
    let hash = Hashtbl.seeded_hash
    let equal = (==)
    let string = Char.escaped
    let sanitize x = x
end

module String = struct
    type t = string
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let string x = x
    let sanitize x = x
    let compare = String.compare
end

module Float = struct
    type t = float
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let string = string_of_float
    let sanitize x = x
end

module Bool = struct
    type t = bool
    let hash = Hashtbl.seeded_hash
    let equal = (==)
    let string = string_of_bool
    let sanitize x = x
end

module Int32 = struct
    type t = int32
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let sanitize x = x
    let string = Int32.to_string
end

module Int64 = struct
    type t = int64
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let sanitize x = x
    let string = Int64.to_string
end

module Nativeint = struct
    type t = nativeint
    let hash = Hashtbl.seeded_hash
    let equal = (=)
    let sanitize x = x      
end

module Option (A : DatType) = struct
    type t = A.t option
    let hash seed = function
        | Some a -> A.hash (Hashtbl.seeded_hash seed `Some) a
        | None -> Hashtbl.seeded_hash seed `None
    let equal x x' = x == x' || match x, x' with
        | Some a, Some a' -> A.equal a a'
        | _ -> false
    let sanitize x = match x with
      | None -> None
      | Some x -> Some (A.sanitize x)
    let string x = match x with
      | None -> "None"
      | Some x -> "Some("^(A.string x)^")"
end

module Tuple2 (A : DatType) (B : DatType) = struct
    type t = A.t * B.t
    let hash seed ( a, b ) = B.hash (A.hash seed a) b
    let equal ( a, b as x ) ( a', b' as x' ) = x == x' || A.equal a a' && B.equal b b'
    let sanitize (a, b) = (A.sanitize a, B.sanitize b)
    let string (a,b) = "("^(A.string a)^","^(B.string b)^")"
end

module Tuple3 (A : DatType) (B : DatType) (C : DatType) = struct
    type t = A.t * B.t * C.t
    let hash seed ( a, b, c ) = C.hash (B.hash (A.hash seed a) b) c
    let equal ( a, b, c as x ) ( a', b', c' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c'
    let sanitize (a, b, c) = (A.sanitize a, B.sanitize b, C.sanitize c)
    let string (a,b,c) = "("^(A.string a)^","^(B.string b)^","^(C.string c)^")"
end

module Tuple4 (A : DatType) (B : DatType) (C : DatType) (D : DatType) = struct
    type t = A.t * B.t * C.t * D.t
    let hash seed ( a, b, c, d ) = D.hash (C.hash (B.hash (A.hash seed a) b) c) d
    let equal ( a, b, c, d as x ) ( a', b', c', d' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d'
    let sanitize (a, b, c, d) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d)
    let string (a,b,c,d) = "("^(A.string a)^","^(B.string b)^","^(C.string c)^","^(D.string d)^")"
end

module Tuple5 (A : DatType) (B : DatType) (C : DatType) (D : DatType) (E: DatType) = struct
    type t = A.t * B.t * C.t * D.t * E.t
    let hash seed ( a, b, c, d, e ) = E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e
    let equal ( a, b, c, d, e as x ) ( a', b', c', d', e' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e'
    let sanitize (a, b, c, d, e) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e)
    let string (a,b,c,d,e) = "("^(A.string a)^","^(B.string b)^","^(C.string c)^","^(D.string d)^","^(E.string e)^")"
end

module Tuple6 (A : DatType) (B : DatType) (C : DatType) (D : DatType) (E: DatType) (F : DatType) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t
    let hash seed ( a, b, c, d, e, f ) = F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f
    let equal ( a, b, c, d, e, f as x ) ( a', b', c', d', e', f' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e' && F.equal f f'
    let sanitize (a, b, c, d, e, f) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f)
    let string (a,b,c,d,e,f) = "("^(A.string a)^","^(B.string b)^","^(C.string c)^","^(D.string d)^","^(E.string e)^","^(F.string f)^")"
end

module Tuple7 (A : DatType) (B : DatType) (C : DatType) (D : DatType) (E: DatType) (F : DatType) (G: DatType) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t
    let hash seed ( a, b, c, d, e, f, g ) = G.hash (F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f) g
    let equal ( a, b, c, d, e, f, g as x ) ( a', b', c', d', e', f', g' as x' ) = x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e' && F.equal f f' && G.equal g g'
    let sanitize (a, b, c, d, e, f, g) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f, G.sanitize g)
    let string (a,b,c,d,e,f,g) = "("^(A.string a)^","^(B.string b)^","^(C.string c)^","^(D.string d)^","^(E.string e)^","^(F.string f)^","^(G.string g)^")"
end

module Tuple8 (A : DatType) (B : DatType) (C : DatType) (D : DatType) (E: DatType) (F : DatType) (G: DatType) (H: DatType) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t
    let hash seed ( a, b, c, d, e, f, g, h ) =
      H.hash (G.hash (F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f) g) h
    let equal ( a, b, c, d, e, f, g, h as x ) ( a', b', c', d', e', f', g', h' as x' ) =
      x == x' || A.equal a a' && B.equal b b' && C.equal c c' && D.equal d d' && E.equal e e' && F.equal f f' && G.equal g g' && H.equal h h'
    let sanitize (a, b, c, d, e, f, g, h) =
      (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f, G.sanitize g, H.sanitize h)
    let string (a,b,c,d,e,f,g,h) =
      "("^(A.string a)^","^(B.string b)^","^(C.string c)^","^(D.string d)^","^(E.string e)^","^(F.string f)^","^(G.string g)^","^(H.string h)^")"
end

module Unit = struct
    type t = unit
    let hash seed () = seed
    let equal = (==)
    let string _ = "()"
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
let makeFunction (type a) (type b) () : (module DatType with type t = a -> b) =
    (module struct
        type t = a -> b
        let equal = (==)
        let hash = Hashtbl.seeded_hash
        let string x = "Types.ml: makeFunction: no string"
        let sanitize x = x (* Lookout! XXX: May not actually sanitize!! *)
    end)

module Sum2 (A : DatType) (B : DatType) = struct
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
    | InL a -> Printf.sprintf "InL (%s)" (A.string a)
    | InR b -> Printf.sprintf "InR (%s)" (B.string b)
end
