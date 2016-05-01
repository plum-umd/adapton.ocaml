open Adapton

module     N = Name
module IntST = SpreadTree.Make(Insts.Nominal)(N)(Types.Int)
module     L = IntST.List
module    AL = IntST.List.Art

let id_rec : L.t -> AL.t =
  let mfn =
    AL.mk_mfn (N.of_string "id_rec") (module L)
      (fun mfn l ->
         let id      :        L.t ->  L.t = mfn.AL.mfn_data
         and id_name : N.t -> L.t -> AL.t = mfn.AL.mfn_nart in
         Printf.printf "id_rec\n%!" ;
         match l with
         | `Nil          -> `Nil
         | `Cons (a,  l) -> `Cons (a, id l)
         | `Name (nm, `Art a) ->
           let nm' = N.(pair (of_string "id_rec") nm) in
           `Name (nm', `Art (id_name nm' (AL.force a)))
         | _ -> assert false)
  in
  mfn.AL.mfn_art

let art : L.t -> L.t =
  fun il -> let nm = N.gensym () in `Name (nm, `Art (AL.cell nm il))

let articulate (il : int list) : L.t =
  List.fold_right (fun i l -> art (`Cons (i, l))) il `Nil

let search : (L.t -> bool) -> L.t -> AL.t option =
  fun p ->
    let rec search l = match l with
      | `Cons (_, l)
      | `Name (_, l) -> search l
      | `Nil         -> None
      | `Art   a     -> let l' = AL.force a in
                        if p l' then Some a else search l'
    in
    search

let mutate : (L.t -> bool) -> (AL.t -> L.t) -> L.t -> unit =
  fun p f l -> match search p l with
    | Some al -> AL.set al (f al)
    | None    -> ()

let rec force_all_al : AL.t -> L.t = fun a -> force_all @@ AL.force a
and     force_all    :  L.t -> L.t = function
    | `Nil as nil  -> nil
    | `Cons (n, l) -> `Cons (n, force_all l)
    | `Art   a     -> force_all_al a
    | `Name (_, l) -> force_all    l

let _ =
  let l0 = articulate [ 2; 8; 5; 2; 1; 7; 1 ] in
  let al = id_rec l0 in
  Printf.printf "%s\n" (L.show (force_all_al al)) ;
  mutate
    (function `Cons (8, _) -> true | _ -> false)
    (fun al -> `Cons (42, art (AL.force al)))
    l0 ;
  Printf.printf "%s\n" (L.show (force_all_al al))
