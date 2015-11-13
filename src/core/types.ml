(** Convenience modules for built-in and other useful types. *)

module Int =
struct
    type t = int [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module List(A : Data.S) =
struct
  type t = A.t list [@@deriving eq, ord, show]
  let rec hash seed = function
    | a::l -> A.hash (hash seed l) a
    | nil  -> Hashtbl.seeded_hash seed nil
  let sanitize = List.map A.sanitize
  let show l =
    if List.length l > 0 then
      let elts = List.fold_right (fun elt a -> (A.show elt)^", "^a) l "" in
      "["^(String.sub elts 0 ((String.length elts)-2))^"]"
    else "[]"
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Char = struct
    type t = char [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module String = struct
    type t = string [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module Float = struct
    type t = float [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module Bool = struct
    type t = bool [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module Int32 = struct
    type t = int32 [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module Int64 = struct
    type t = int64 [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let sanitize x = x
end

module Nativeint = struct
    type t = nativeint [@@deriving eq, ord, show]
    let hash = Hashtbl.seeded_hash
    let show = Nativeint.to_string
    let compare = Nativeint.compare
    let pp fmt s = Format.fprintf fmt "%s" (show s)
    let equal = (=)
    let sanitize x = x      
end

module Option (A : Data.S) = struct
    type t = A.t option [@@deriving eq, ord, show]
    let hash seed = function
        | Some a -> A.hash (Hashtbl.seeded_hash seed `Some) a
        | None -> Hashtbl.seeded_hash seed `None
    let sanitize x = match x with
      | None -> None
      | Some x -> Some (A.sanitize x)
end

module Tuple2(A : Data.S)(B : Data.S) = struct
    type t = A.t * B.t [@@deriving eq, ord, show]
    let hash seed ( a, b ) = B.hash (A.hash seed a) b
    let sanitize (a, b) = (A.sanitize a, B.sanitize b)
end

module Tuple3 (A : Data.S) (B : Data.S) (C : Data.S) = struct
    type t = A.t * B.t * C.t [@@deriving eq, ord, show]
    let hash seed ( a, b, c ) = C.hash (B.hash (A.hash seed a) b) c
    let sanitize (a, b, c) = (A.sanitize a, B.sanitize b, C.sanitize c)
end

module Tuple4 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) = struct
    type t = A.t * B.t * C.t * D.t [@@deriving eq, ord, show]
    let hash seed ( a, b, c, d ) = D.hash (C.hash (B.hash (A.hash seed a) b) c) d
    let sanitize (a, b, c, d) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d)
end

module Tuple5 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t [@@deriving eq, ord, show]
    let hash seed ( a, b, c, d, e ) = E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e
    let sanitize (a, b, c, d, e) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e)
end

module Tuple6 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t [@@deriving eq, ord, show]
    let hash seed ( a, b, c, d, e, f ) = F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f
    let sanitize (a, b, c, d, e, f) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f)
end

module Tuple7 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) (G: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t [@@deriving eq, ord, show]
    let hash seed ( a, b, c, d, e, f, g ) = G.hash (F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f) g
    let sanitize (a, b, c, d, e, f, g) = (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f, G.sanitize g)
end

module Tuple8 (A : Data.S) (B : Data.S) (C : Data.S) (D : Data.S) (E: Data.S) (F : Data.S) (G: Data.S) (H: Data.S) = struct
    type t = A.t * B.t * C.t * D.t * E.t * F.t * G.t * H.t [@@deriving eq, ord, show]
    let hash seed ( a, b, c, d, e, f, g, h ) =
      H.hash (G.hash (F.hash (E.hash (D.hash (C.hash (B.hash (A.hash seed a) b) c) d) e) f) g) h
    let sanitize (a, b, c, d, e, f, g, h) =
      (A.sanitize a, B.sanitize b, C.sanitize c, D.sanitize d, E.sanitize e, F.sanitize f, G.sanitize g, H.sanitize h)
end

module Unit = struct
    type t = unit [@@deriving eq, ord, show]
    let hash seed () = Hashtbl.seeded_hash seed "Adapton.Types.Unit.hash"
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
  [@@deriving eq, ord, show]
  let hash seed = function
    | InL a -> A.hash seed a
    | InR b -> B.hash seed b
  let sanitize = function
    | InL a -> InL (A.sanitize a)
    | InR b -> InR (B.sanitize b)
end
