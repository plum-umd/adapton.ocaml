include Adapton_lib

module type Store = sig
    type ('a, 'b) sto
    val mt : ('a, 'b) sto
    val lookup : ('a, 'b) sto -> 'a -> 'b
    val ext : ('a, 'b) sto -> 'a -> 'b -> ('a, 'b) sto
  end

module AssocStore : Store = struct
  type ('a, 'b) sto = ('a * 'b) list

  let mt = []

  let lookup : ('a, 'b) sto -> 'a -> 'b =
    fun s x -> List.assoc x s

  let ext : ('a, 'b) sto -> 'a -> 'b -> ('a, 'b) sto =
    fun s x v ->
    (x, v) :: s
end

open AssocStore
type store = (string, int) sto

type aexpr =
  | Int of int
  | Plus of aexpr * aexpr
  | Minus of aexpr * aexpr
  | Times of aexpr * aexpr
  | Var of string

let rec aevals s = function
  | Var x -> Int (lookup s x)
  | Plus (Int n, Int m) -> Int (n+m)
  | Plus (Int n, b) -> Plus(Int n, aevals s b)
  | Plus (a, b) -> Plus((aevals s a), b)

type bexpr =
  | True
  | False
  | Not of bexpr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Eq of aexpr * aexpr
  | Leq of aexpr * aexpr

let rec bevals s = function
  | Leq (Int n, Int m) -> if n<=m then True else False
  | Leq (Int n, a) -> Leq (Int n, aevals s a)
  | Leq (a1, a2) -> Leq (aevals s a1, a2)
  | _ -> failwith "Oops!"

type cmd =
  | Skip
  | Assign of string * aexpr
  | Seq of cmd * cmd
  | If of bexpr * cmd * cmd
  | While of bexpr * cmd

let rec cevals (s, c) = match c with
  | Assign (x, Int n) -> (ext s x n, Skip)
  | Assign (x, a) -> (s, Assign (x, aevals s a))
  | Seq (Skip, c2) -> (s, c2)
  | Seq (c1, c2) ->
     let s', c' = cevals (s, c1) in
     (s', Seq (c', c2))
  | If (True, c1, c2) -> (s, c1)
  | If (False, c1, c2) -> (s, c2)
  | If (b, c1, c2) ->
     (s, (If ((bevals s b), c1, c2)))
  | While (b, c) as w -> (s, If (b, Seq(c, w), Skip))

let rec aeval s = function
  | Int n -> n
  | Plus (e1, e2) -> (aeval s e1) + (aeval s e2)
  | Minus (e1, e2) -> (aeval s e1) - (aeval s e2)
  | Times (e1, e2) -> (aeval s e1) * (aeval s e2)
  | Var x -> lookup s x

let rec beval s = function
  | True -> true
  | False -> false
  | Not b -> not (beval s b)
  | And (b1, b2) -> (beval s b1) && (beval s b2)
  | Or (b1, b2) -> (beval s b1) || (beval s b2)
  | Leq (a1, a2) -> (aeval s a1) <= (aeval s a2)
  | Eq (a1, a2) -> (aeval s a1) = (aeval s a2)

let rec ceval s = function
  | Skip -> s
  | Assign (x, a) -> ext s x (aeval s a)
  | Seq (c0, c1) -> ceval (ceval s c0) c1
  | If (b, c0, c1) ->
     (match beval s b with
        true -> ceval s c0
      | false -> ceval s c1)
  (*  | (While (b, c)) as w ->
     (match beval s b with
true -> ceval (ceval s c) w
      | false -> s)*)
  | (While (b, c)) as w -> ceval s (If (b, Seq(c, w), Skip))
;;

  aeval (ext (ext mt "x" 3) "y" 4)
        (Plus(Var "x", Times((Var "y"), (Int 3))));;

  beval (ext (ext mt "x" 3) "y" 4)
        (Eq((Plus(Var "x", Times((Var "y"), (Int 3)))),
            (Plus(Var "y", Times((Var "x"), (Int 3))))));;

  let fact =
    Seq(Assign ("n", Int 5),
        Seq(Assign ("f", Int 1),
            While (Leq(Int 1, Var "n"),
                   Seq(Assign("f", Times(Var "f", Var "n")),
                       Assign("n", Minus(Var "n", Int 1))))));;

    lookup (ceval mt fact) "f";;
