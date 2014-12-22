(* Goals:

   basic goals:
   -- movement
   -- writing (creating new formula; eventually, type-checking them too)
   -- reading (evaluating formula)
   -- visual display (ascii text to terminal)

   intermediate goals:
   -- integration with Adapton
   -- load/save to disk
   -- replay test scripts

   longer-term goals:
   -- type checker
   -- explore circular references:
      -- loan amortization demo
      -- game of life demo

   Questions:
   -- sorting (?) -- how does that fit into this demo?
   -- lazy lists -- are these needed / relevant here?
*)

module type INTERP = functor (Ast : Ast.S) -> sig
  type cell
  type db
  type cur

  val empty : int * int * int -> db
  val eval_ : db -> Ast.sht -> Ast.formula -> Ast.const Ast.A.thunk
  val eval  : cur -> Ast.formula -> Ast.const Ast.A.thunk

  type 'a fold_body = ( cur -> 'a -> 'a )

  type 'a foldees = { fold_cell      : 'a fold_body ;
                      fold_row_begin : 'a fold_body ;
                      fold_row_end   : 'a fold_body ; }

  val fold_region  : Ast.absolute_region -> db -> 'a foldees -> 'a -> 'a
  val print_region : Ast.absolute_region -> db -> out_channel -> unit

  (* Cursor-based interaction: *)
  val cursor  : Ast.pos -> db -> cur
  val move    : Ast.nav_cmd -> cur -> cur
  val get_pos : cur -> Ast.pos
  val get_frm : cur -> Ast.formula
  val get_val : cur -> Ast.const
(*
  val load  : string -> db
  val save  : db -> string -> unit
*)

  val get_db : cur -> db

  val read   : cur -> Ast.const
  val write  : Ast.mut_cmd -> cur -> cur

  val scramble       : cur -> unit
  val scramble_dense : cur -> unit
  val scramble_one   : cur -> unit
end

module Make : INTERP = functor (Ast : Ast.S) -> struct
  open Ast

  exception NYI

  module Coord = struct
    type t = pos
    let compare c1 c2 = compare c1 c2
    let equals c1 c2 = (c1 = c2)
  end

  module Mp = Map.Make(Coord)

  type cell = { cell_frm : formula A.thunk ;
                mutable cell_val : const   A.thunk ;
                (* Invariant: if ! Global.stateless_semantics then cell_val is Undef *)
              }

  (* these mutable field below for cells need not be instrumented by
     adapton bc the changes will only be monotonic (will only add new
     stuff, will not alter the associations/identity of old stuff). *)
  type db = { nshts  : int ;
              ncols  : int ;
              nrows  : int ;
              mutable cells : cell Mp.t ;
              mutable eval : sht -> formula -> const A.thunk ;
            }

  type cur = { db : db ;
               pos : pos ; }

  let get_db cur = cur.db

(*
  let load filename =
    raise NYI

  let save db filename =
    raise NYI
*)

  let cursor pos db = { pos = pos ; db = db }

  let get_pos cur = cur.pos

  let get_frm cur =
    try A.force (Mp.find cur.pos cur.db.cells).cell_frm with
      | Not_found -> F_const Undef

  let sht_of_reg (s,_) = s
  let sht_of_pos (s,_) = s

  let get_val_ eval cur =
    try
      let cell = Mp.find cur.pos cur.db.cells in
      if ! Global.stateless_eval then
        eval (sht_of_pos (get_pos cur)) (A.force cell.cell_frm)
      else
        cell.cell_val
    with
      | Not_found -> A.const Undef

  let get_val cur = A.force (get_val_ cur.db.eval cur)

  let pos_is_valid : pos -> db -> bool =
    fun (s,(c,r)) {nshts;ncols;nrows} ->
      ( s > 0 && c > 0 && r > 0 &&
          s <= nshts && c <= ncols && r <= nrows )

  (* move with no bounds checks. *)
  let move_raw navcmd = fun cur ->
    { cur with pos =
        let pos = cur.pos in
        match navcmd with
          | C_goto (Abs pos) -> pos
          | C_goto (Lcl lc)  -> (fst pos, lc)
          | C_prev (Nav_sht) -> (fst pos - 1, snd pos)
          | C_next (Nav_sht) -> (fst pos + 1, snd pos)
          | C_next (Nav_col) -> (fst pos, (fst (snd pos) + 1, (snd (snd pos))))
          | C_prev (Nav_col) -> (fst pos, (fst (snd pos) - 1, (snd (snd pos))))
          | C_prev (Nav_row) -> (fst pos, (fst (snd pos), (snd (snd pos)) - 1))
          | C_next (Nav_row) -> (fst pos, (fst (snd pos), (snd (snd pos)) + 1))
    }

  let move navcmd : cur -> cur = fun cur ->
    let cur' = move_raw navcmd cur in
    { cur with pos =
        if pos_is_valid cur'.pos cur.db
        then cur'.pos else cur.pos }

  type 'a fold_body = ( cur -> 'a -> 'a )
  type 'a foldees = { fold_cell      : 'a fold_body ;
                      fold_row_begin : 'a fold_body ;
                      fold_row_end   : 'a fold_body ; }

  (* Folding a region: move in row-major order through the region.
     uses the foldee callbacks to fold a parametric accumulator
     through the region's structure. *)
  let fold_region : Ast.absolute_region -> db -> 'a foldees -> 'a -> 'a =

    fun (sht, l_reg) db foldees x ->
      (* accept any region, convert to TL-BR form *)
      let (lc1,max_col,max_row) = match l_reg with
      | ((c1,r1), (c2, r2)) when c1>=c2&&r1>=r2 -> ((c2,r2),c1,r1)
      | ((c1,r1), (c2, r2)) when c1<=c2&&r1>=r2 -> ((c1,r2),c2,r1)
      | ((c1,r1), (c2, r2)) when c1>=c2&&r1<=r2 -> ((c2,r1),c1,r2)
      | ((c1,r1), (c2, r2)) when c1<=c2&&r1<=r2 -> ((c1,r1),c2,r2)
      | _ -> failwith "impossible"
      in
      let cur =
        cursor (sht, lc1) db
      in
      let within_region {pos=(s,(c,r))} =
        ((c <= max_col) && (r <= max_row))
      in
      let rec loop_rows : cur -> 'a -> 'a = fun cur x ->
        (* fold cols on current row *)
        let rec loop_cols : cur -> 'a -> (cur * 'a) = fun cur x ->
          if within_region cur then
            let x   = foldees.fold_cell cur x in
            let cur = move_raw (Ast.C_next Ast.Nav_col) cur in
            loop_cols cur x
          else
            cur, x
        in
        (* fold all remaining rows: *)
        if not (within_region cur) then x
        else
          let   x = foldees.fold_row_begin cur x in
          let _,x = loop_cols cur x in
          let   x = foldees.fold_row_end cur x in
          let cur = move_raw (Ast.C_next Ast.Nav_row) cur in
          loop_rows cur x
      in
      loop_rows cur x

  (* lookup and evaluate an absolute coordinate. *)
  let lookup_cell : db -> pos -> cell =
    fun db pos ->
      try (Mp.find pos db.cells) with
        | Not_found -> begin
            let undef_frm = A.const (F_const Undef) in
            let undef_cell = { cell_frm = undef_frm ;
                               cell_val = A.const Undef } in
            (* Monotonic side effect:
               create and remember a new formula, initially holding Undef. *)
            db.cells <- Mp.add pos undef_cell db.cells ;
            undef_cell
          end

  (* -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- *)
  (* -- formula evaluation -- *)
  (* ADAPTON memoizes this function, based on the sheet and formula arguments. *)
  let eval_ db =
    let eval_memoized = A.memo2
      ~inp2_equal:Ast.frm_equal
      ~inp2_hash:Ast.frm_hash
      begin fun eval_memo sht (frm : formula) ->

        let to_app_form : 'a list -> ('a list -> 'a list)
          = fun xs -> (fun xs_tail -> xs @ xs_tail)
        in

        let snoc : ('a list -> 'a list) -> 'a -> ('a list -> 'a list)
          = fun xs y -> (fun tl -> xs (y :: tl))
        in

        let eval_memo_ sht frm =
          eval_memo sht (A.force frm)
        in

        (* evaluate given formula *)
        match frm with
          | F_const c -> c
          | F_paren f -> A.force (eval_memo_ sht f)
          | F_coord coord -> A.force (get_val_ eval_memo {pos=(absolute sht coord); db=db})
          | F_func(f,r) ->
              let r = match r with
                | R_lcl lr -> (sht,lr)
                | R_abs reg -> reg
              in
              let cells = fold_region r db {
                fold_row_begin = begin fun cur x -> x end ;
                fold_row_end   = begin fun cur x -> x end ;
                fold_cell      = begin fun cur cells ->
                  snoc cells (get_val_ eval_memo cur) end
              } (to_app_form [])
              in
              begin match cells [] with
                | []    -> Undef
                | x::xs ->
                    let x = A.force x in
                    List.fold_right begin fun x y ->
                      let x = A.force x in
                      match f, x, y with
                        | Fn_sum,  Num x, Num y -> Num ( Num.add_num x y )
                        | Fn_prod, Num x, Num y -> Num ( Num.mult_num x y )
                        | Fn_max,  Num x, Num y -> Num ( Num.max_num x y )
                        | Fn_min,  Num x, Num y -> Num ( Num.min_num x y )
                        | _,       Fail,  _     -> Fail
                        | _,       _   ,  Fail  -> Fail
                        | _,       Undef, _     -> Undef
                        | _,       _,     Undef -> Undef
                    end xs x
              end

          | F_binop(bop,f1,f2) -> begin
              let c1 = A.force (eval_memo_ sht f1) in
              let c2 = A.force (eval_memo_ sht f2) in
              try
                begin match bop, c1, c2 with
                  | Bop_add, Num n1, Num n2 -> Num (Num.add_num n1 n2)
                  | Bop_sub, Num n1, Num n2 -> Num (Num.sub_num n1 n2)
                  | Bop_div, Num n1, Num n2 -> Num (Num.div_num n1 n2)
                  | Bop_mul, Num n1, Num n2 -> Num (Num.mult_num n1 n2)
                  | Bop_min, Num n1, Num n2 -> Num (Num.min_num n1 n2)
                  | Bop_max, Num n1, Num n2 -> Num (Num.max_num n1 n2)
                  | _,       Fail,   _      -> Fail
                  | _,       _,      Fail   -> Fail
                  | _,       Undef,  _      -> Undef
                  | _,       _,      Undef  -> Undef
                end
              with
                | Failure _ -> Fail
            end
      end
    in
    eval_memoized

  exception Not_yet_back_patched

  let empty (nshts,ncols,nrows) =
    let db =
      { nshts = nshts ;
        ncols = ncols ;
        nrows = nrows ;
        cells = Mp.empty ;
        eval  = (fun _ _ -> raise Not_yet_back_patched) ;
      }
    in
    db.eval <- eval_ db ;
    db

  let eval cur = cur.db.eval (sht_of_pos (get_pos cur))

  (* -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- *)
  (* -- pretty printing -- *)
  let print_region : Ast.absolute_region -> db -> out_channel -> unit =
    fun reg db out ->
      let ps = print_string in
      fold_region reg db {
        fold_row_begin = begin fun cur x -> ()      end ;
        fold_row_end   = begin fun _ _   -> ps "\n" end ;
        fold_cell =
          begin fun cur _ ->
            let c = get_val cur in
            ps "| " ;
            Printf.fprintf out "%10s" (Pretty.string_of_const c) ;
            ps " |"
          end } ()

  let read cur = get_val cur

  let update_cell_frm cur cell frm =
    if A.is_incremental then (
      A.update_const cell.cell_frm frm ;
      cell.cell_val <- A.thunk begin fun _ ->
        if (! Global.stateless_eval ) then
          Undef
        else
          A.force (cur.db.eval (sht_of_pos cur.pos) frm)
      end
    )
    else
      cur.db.cells <-
        Mp.add cur.pos {
          cell_frm=(A.const frm);
          cell_val=
            if ! Global.stateless_eval
            then (A.const Undef)
            else cur.db.eval (sht_of_pos cur.pos) frm ;
        } cur.db.cells

  let random_const () =
    (Ast.F_const (Ast.Num (Num.num_of_int (Random.int 10000))))

  let scramble_cell cur cell =
    update_cell_frm cur cell (random_const ())

   let scramble_one cur =
    let db = cur.db in
    let rnd max = (Random.int (max - 1)) + 1 in
    let s = sht_of_pos (get_pos cur) in
    let pos =
      let s = rnd s in
      let c = rnd db.ncols in
      let r = rnd db.nrows in
      (s, (c,r))
    in
    let cell = lookup_cell db pos in
    scramble_cell cur cell

  (* TODO: code reduction, both internally and with scramble_dense below *)
  let scramble cur =
    let db = cur.db in
    for s = 1 to db.nshts do
      for r = 1 to db.ncols do
        for c = 1 to db.nrows do
          let cell = lookup_cell db (s,(c,r)) in
          if s <= 1 || r <= 1 || c <= 1 then
            scramble_cell {cur with pos=(s,(r,c))} cell
          else
            let rnd max = (Random.int (max - 1)) + 1 in
            (* This scrambles using any PREVIOUS sheet, and
            the first sheet is filled with random consts above,
            so there can be no loops in the computation graph *)
            let s1, s2 = rnd s, rnd s in
            let c1, c2 = rnd db.ncols, rnd db.ncols in
            let r1, r2 = rnd db.nrows, rnd db.nrows in
            let binop_chance = match !Global.func_arity with
              | "only_binop" -> 100
              | "more_binop" -> 75
              | "all_arities" -> 50
              | "more_multi" -> 25
              | "only_multi" -> 0
              (* only the above should be possible *)
              | _ -> failwith "unrecognized function arity"
            in
             let fun_chance = match !Global.func_type with
              | "only_fun" -> 100
              | "more_fun" -> 75
              | "all_types" -> 50
              | "more_cmp" -> 25
              | "only_cmp" -> 0
              (* only the above should be possible *)
              | _ -> failwith "unrecognized function type"
            in
            if (Random.int 100) < binop_chance then
              if (Random.int 100) < fun_chance then
                (* binary functions *)
                let b = match Random.int 4 with
                  | 0 -> Ast.Bop_add
                  | 1 -> Ast.Bop_sub
                  | 2 -> Ast.Bop_div
                  | _ -> Ast.Bop_mul
                in
                let f1 = Ast.F_coord (Abs (s1, (c1, r1))) in
                let f2 = Ast.F_coord (Abs (s2, (c2, r2))) in
                let f3 = Ast.F_binop (b, (memo_frm f1), (memo_frm f2)) in
                update_cell_frm {cur with pos=(s,(r,c))} cell f3
              else
                (* binary compares *)
                let b = match Random.int 2 with
                  | 0 -> Ast.Bop_min
                  | _ -> Ast.Bop_max
                in
                let f1 = Ast.F_coord (Abs (s1, (c1, r1))) in
                let f2 = Ast.F_coord (Abs (s2, (c2, r2))) in
                let f3 = Ast.F_binop (b, (memo_frm f1), (memo_frm f2)) in
                update_cell_frm {cur with pos=(s,(r,c))} cell f3
            else
              if (Random.int 100) < fun_chance then
                (* regional functions *)
                let f = match Random.int 5 with
                  | 0 -> Ast.Fn_prod
                  | _ -> Ast.Fn_sum
                in
                let fn = Ast.F_func (f, (R_abs (s1, ((c1,r1),(c2,r2))))) in
                update_cell_frm {cur with pos=(s,(r,c))} cell fn
              else
                (* regional compares *)
                let f = match Random.int 2 with
                  | 0 -> Ast.Fn_max
                  | _ -> Ast.Fn_min
                in
                let fn = Ast.F_func (f, (R_abs (s1, ((c1,r1),(c2,r2))))) in
                update_cell_frm {cur with pos=(s,(r,c))} cell fn
        done
      done
    done

  let scramble_dense cur =
    let db = cur.db in
    for s = 1 to db.nshts do
      for r = 1 to db.ncols do
        for c = 1 to db.nrows do
          let cell = lookup_cell db (s,(c,r)) in
          if s <= 1 || r <= 1 || c <= 1 then
            scramble_cell {cur with pos=(s,(r,c))} cell
          else
            let rnd max = (Random.int (max - 1)) + 1 in
            let c1, c2 = rnd db.ncols, rnd db.ncols in
            let r1, r2 = rnd db.nrows, rnd db.nrows in
            let binop_chance = match !Global.func_arity with
              | "only_binop" -> 100
              | "more_binop" -> 75
              | "all_arities" -> 50
              | "more_multi" -> 25
              | "only_multi" -> 0
              (* only the above should be possible *)
              | _ -> failwith "unrecognized function arity"
            in
             let fun_chance = match !Global.func_type with
              | "only_fun" -> 100
              | "more_fun" -> 75
              | "all_types" -> 50
              | "more_cmp" -> 25
              | "only_cmp" -> 0
              (* only the above should be possible *)
              | _ -> failwith "unrecognized function type"
            in
            if (Random.int 100) < binop_chance then
              if (Random.int 100) < fun_chance then
                (* binary functions *)
                let b = match Random.int 4 with
                  | 0 -> Ast.Bop_add
                  | 1 -> Ast.Bop_sub
                  | 2 -> Ast.Bop_div
                  | _ -> Ast.Bop_mul
                in
                let f1 = Ast.F_coord (Abs (s - 1, (c1, r1))) in
                let f2 = Ast.F_coord (Abs (s - 1, (c2, r2))) in
                let f3 = Ast.F_binop (b, (memo_frm f1), (memo_frm f2)) in
                update_cell_frm {cur with pos=(s,(r,c))} cell f3
              else
                (* binary compares *)
                let b = match Random.int 2 with
                  | 0 -> Ast.Bop_min
                  | _ -> Ast.Bop_max
                in
                let f1 = Ast.F_coord (Abs (s - 1, (c1, r1))) in
                let f2 = Ast.F_coord (Abs (s - 1, (c2, r2))) in
                let f3 = Ast.F_binop (b, (memo_frm f1), (memo_frm f2)) in
                update_cell_frm {cur with pos=(s,(r,c))} cell f3
            else
            (* let's skip the region functions, they're reeeeeeealy slow
              if (Random.int 100) < fun_chance then
                (* regional functions *)
                let f = match Random.int 5 with
                  | 0 -> Ast.Fn_prod
                  | _ -> Ast.Fn_sum
                in
                let fn = Ast.F_func (f, (R_abs (s - 1, ((c1,r1),(c2,r2))))) in
                update_cell_frm {cur with pos=(s,(r,c))} cell fn
              else
            *)
                (* regional compares *)
                let f = match Random.int 2 with
                  | 0 -> Ast.Fn_max
                  | _ -> Ast.Fn_min
                in
                let fn = Ast.F_func (f, (R_abs (s - 1, ((c1,r1),(c2,r2))))) in
                update_cell_frm {cur with pos=(s,(r,c))} cell fn
        done
      done
    done

  let write mutcmd cur =
    begin match mutcmd with
      | C_set frm -> begin
          let cell = lookup_cell cur.db cur.pos in
          update_cell_frm cur cell (A.force frm)
        end
      | Ast.C_scramble Ast.Sf_sparse -> scramble cur
      | Ast.C_scramble Ast.Sf_dense  -> scramble_dense cur
      | Ast.C_scramble Ast.Sf_one    -> scramble_one   cur
    end
    ; cur

end
