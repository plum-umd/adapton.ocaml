(*
From adapton_structures/SpreadTree.ml
 
let list_map 
(op_nm : Name.t)
(op : St.Data.t -> St.Data.t)
: St.List.Data.t -> St.List.Data.t = 
let fnn = Name.pair (Name.gensym "list_map") op_nm in
let mfn = LArt.mk_mfn fnn
      (module St.List.Data)
      (fun r list -> 
        let list_map = r.LArt.mfn_data in
        match list with
        | `Nil -> `Nil
        | `Cons(x, xs) -> `Cons(op x, list_map xs)
        | `Art(a) -> list_map (LArt.force a)
        | `Name(nm, xs) -> 
          let nm1, nm2 = Name.fork nm in
          `Name(nm1, `Art(r.LArt.mfn_nart nm2 xs))
      )
    in
    fun list -> mfn.LArt.mfn_data list
*)

(*
1,2,3,4,2,3
1,1,1,1,0,0

let rec unique xs seen =
match xs with
   | [] -> []
   | x :: ys ->
     if (find seen x) then 0 else 1) ::
     (unique ys (insert x seen))

let rec unique xs seen =
match xs with
   | [] -> []
   | (x, n) :: ys ->
     if (find seen x) then 0 else 1) ::
     (unique ys (insert n x seen))
*)
