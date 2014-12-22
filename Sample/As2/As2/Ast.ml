
module type S = sig
  module A : AdaptonUtil.PolyAPI.S

  type col = int
  type row = int
  type sht = int

  (** -- commands -- **)

  type cmd =
    | C_help | C_exit
    | C_nav of nav_cmd
    | C_mut of mut_cmd
    | C_seq of cmd * cmd
    | C_print
    | C_repeat of formula' * cmd

  and scramble_flags =
    | Sf_sparse
    | Sf_dense
    | Sf_one

  and mut_cmd =
    | C_set of formula'
    | C_scramble of scramble_flags

  and nav_cmd =
    | C_next of nav_thing
    | C_prev of nav_thing
    | C_goto of coord

  and nav_thing = Nav_row | Nav_col | Nav_sht

  (** - - Coordinates, Regions - - **)

  and local_coord = col * row
  and absolute_coord = sht * local_coord
  and pos = absolute_coord
      (* pos is nice short-hand; pos is a "cononical form" for
         coordinates within the interpreter. *)
  and coord =
    | Lcl of local_coord
    | Abs of absolute_coord

  and local_region = local_coord * local_coord
  and absolute_region = sht * local_region
  and region =
    | R_lcl of local_region
    | R_abs of absolute_region

  (** - - Formulas - - **)

  and formula =
    | F_func of func * region
    | F_binop of binop * formula' * formula'
    | F_const of const
    | F_coord of coord
    | F_paren of formula'

  and formula' = formula A.thunk

  (* binary operations *)
  and binop =
    (* function type *)
    | Bop_add
    | Bop_sub
    | Bop_div
    | Bop_mul
    (* compare type *)
    | Bop_min
    | Bop_max

  (* operations over regions *)
  and func =
    (* function type *)
    | Fn_prod
    | Fn_sum
    (* compare type *)
    | Fn_max
    | Fn_min

  and const =
    | Num   of Num.num (* ocaml standard library; arbitrary-precision numbers. *)
    | Fail
    | Undef

  val absolute : sht -> coord -> pos
  val frm_equal : formula -> formula -> bool
  val frm_hash : int -> formula -> int
  val memo_frm : formula -> formula'

  module Pretty : sig
    val string_of_const : const -> string
    val pp_cmd : cmd -> unit
    val pp_nav_cmd : nav_cmd -> unit
    val pp_nav_thing : nav_thing -> unit
    val pp_local_coord : local_coord -> unit
    val pp_pos : pos -> unit
    val pp_coord : coord -> unit
    val pp_local_region : local_region -> unit
    val pp_region : region -> unit
    val pp_mut_cmd : mut_cmd -> unit
    val pp_formula : formula -> unit
    val pp_formula' : formula' -> unit
    val pp_binop : binop -> unit
    val pp_func : func -> unit
  end
end

module Make (AA : AdaptonUtil.Signatures.AType) : S
  with module A = AdaptonUtil.PolyAPI.Make (AA) = struct

  module A = AdaptonUtil.PolyAPI.Make (AA)

  type col = int
  type row = int
  type sht = int

  (** -- commands -- **)

  type cmd =
    | C_help | C_exit
    | C_nav of nav_cmd
    | C_mut of mut_cmd
    | C_seq of cmd * cmd
    | C_print
    | C_repeat of formula' * cmd

  and scramble_flags =
    | Sf_sparse
    | Sf_dense
    | Sf_one

  and mut_cmd =
    | C_set of formula'
    | C_scramble of scramble_flags

  and nav_cmd =
    | C_next of nav_thing
    | C_prev of nav_thing
    | C_goto of coord

  and nav_thing = Nav_row | Nav_col | Nav_sht

  (** - - Coordinates, Regions - - **)

  and local_coord = col * row
  and absolute_coord = sht * local_coord
  and pos = absolute_coord
      (* pos is nice short-hand; pos is a "cononical form" for
         coordinates within the interpreter. *)
  and coord =
    | Lcl of local_coord
    | Abs of absolute_coord

  and local_region = local_coord * local_coord
  and absolute_region = sht * local_region
  and region =
    | R_lcl of local_region
    | R_abs of absolute_region

  (** - - Formulas - - **)

  and formula =
    | F_func of func * region
    | F_binop of binop * formula' * formula'
    | F_const of const
    | F_coord of coord
    | F_paren of formula'

  and formula' = formula A.thunk

  (* binary operations *)
  and binop =
    (* function type *)
    | Bop_add
    | Bop_sub
    | Bop_div
    | Bop_mul
    (* compare type *)
    | Bop_min
    | Bop_max

  (* operations over regions *)
  and func =
    (* function type *)
    | Fn_prod
    | Fn_sum
    (* compare type *)
    | Fn_max
    | Fn_min

  and const =
    | Num   of Num.num (* ocaml standard library; arbitrary-precision numbers. *)
    | Fail
    | Undef

  (* create an absolute coord *)
  let absolute : sht -> coord -> pos
    = fun s -> function
      | Abs(s',(c,r)) -> (s',(c,r))
      | Lcl(c,r)      -> (s,(c,r))

  let frm_equal f1 f2 = match f1, f2 with
    | F_func (f1, reg1),      F_func (f2, reg2)      -> f1 = f2 && reg1 = reg2
    | F_binop (b1, f11, f12), F_binop (b2, f21, f22) -> b1 = b2 && (A.equal f11 f21) && (A.equal f12 f22)
    | F_const (Num n1),       F_const (Num n2)       -> (Num.compare_num n1 n2) = 0
    | F_const (Fail | Undef), _                      -> false
    | _,                      F_const (Fail | Undef) -> false
    | F_coord c1,             F_coord c2             -> c1 = c2
    | F_paren f1,             F_paren f2             -> A.equal f1 f2
    | _,                      _                      -> false

(*
seeded_hash (seeded_hash seed "F_const") "Fail"
seeded_hash (seeded_hash (seeded_hash (seeded_hash seed "F_const") "Num")) n
*)

  open Hashtbl

  let rec frm_hash x f =
    let my_hash x thing =
      seeded_hash_param 10 100 x thing
    in
    match f with
      | F_func (f, reg) ->
          let x = my_hash x f in
          let x = my_hash x reg in
        x
    | F_binop (b, f1, f2) ->
        let x = my_hash x b in
        let x = A.hash x f1 in
        let x = A.hash x f2 in
        x
    | F_paren f        -> my_hash x f
    | F_coord c        -> my_hash x f
    | F_const (Num n)  -> my_hash x n
    | F_const Fail     -> my_hash x "F"
    | F_const Undef    -> my_hash x "U"

  (* hash-cons'd formulae: *)
  let memo_frm : formula -> formula' =
    let f =
      A.memo ~inp_equal:frm_equal ~inp_hash:frm_hash begin
        fun f frm -> frm end
    in f

  module Pretty = struct

    let string_of_const = function
      | Num n -> Num.approx_num_exp 10 n
      | Fail  -> "#fail"
      | Undef -> "#undef"

    let ps = print_string

    let rec pp_cmd = function
      | C_help -> ps "help"
      | C_exit -> ps "exit"
      | C_nav c -> pp_nav_cmd c
      | C_mut c -> pp_mut_cmd c
      | C_print -> ps "print"
      | C_seq (c1, c2) -> pp_cmd c1 ; ps "; " ; pp_cmd c2
      | C_repeat (f, c) ->
          ps "repeat " ;
          pp_formula' f ; ps " do " ;
          pp_cmd c ;
          ps " done"

    (** - - Navigation / Focus - - **)

    and pp_nav_cmd = function
      | C_next nt -> ps "next " ; pp_nav_thing nt
      | C_prev nt -> ps "prev " ; pp_nav_thing nt
      | C_goto c -> ps "goto " ; pp_coord c

    and pp_nav_thing = function
      | Nav_row -> ps "row"
      | Nav_col -> ps "col"
      | Nav_sht -> ps "sheet"

    (** - - Coordinates, Regions - - **)

    and pp_local_coord = fun (col,row) ->
      ps (String.make 1 (Char.chr ((Char.code 'A') + col - 1))) ;
      (* ps "[" ; ps (string_of_int col) ; ps "]" ; *)
      ps (string_of_int row)

    and pp_pos (s,lc) = pp_coord (Abs(s,lc))
    and pp_coord = function
      | Lcl lc -> pp_local_coord lc
      | Abs (s,lc) ->
          ps "sheet" ; ps (string_of_int s) ; ps "!" ;
          pp_local_coord lc

    and pp_local_region = fun (lc1,lc2) ->
      pp_local_coord lc1 ; ps ":" ;
      pp_local_coord lc2

    and pp_region = function
    | R_lcl lr -> pp_local_region lr
    | R_abs (s,lr) ->  ps (string_of_int s) ; pp_local_region lr

    (** - - Formulas - - **)

    and pp_mut_cmd = function
      | C_set f -> ps "=" ; pp_formula' f ; ps "."
      | C_scramble Sf_sparse -> ps "scramble"
      | C_scramble Sf_dense  -> ps "scrambled"
      | C_scramble Sf_one    -> ps "scramble1"

    and pp_formula = function
      | F_func (f,r) -> pp_func f ; ps "(" ; pp_region r ; ps ")"
      | F_binop (b,f1,f2) as f ->
          if !Global.print_ast_db
          then
            ps ("##"^(string_of_int (frm_hash 0 f))^"[")
          else ()
          ;
          pp_formula' f1 ; ps " " ;
          pp_binop b ; ps " " ; pp_formula' f2 ;
          if !Global.print_ast_db then ps "]" else ()
      | F_const c -> ps (string_of_const c)
      | F_coord c -> pp_coord c
      | F_paren f -> ps "(" ; pp_formula' f ; ps ")"

    and pp_formula' f =
      if !Global.print_ast_db then
        ps ("#"^(string_of_int ( match A.id f with Some n -> n | None -> -1 ) )^"[")
      else
        ()
      ;
      pp_formula ( A.force f ) ;
      if !Global.print_ast_db then ps "]" else ()

    and pp_binop = function
      | Bop_add -> ps "+"
      | Bop_sub -> ps "-"
      | Bop_div -> ps "/"
      | Bop_mul -> ps "*"
      | Bop_min -> ps "min"
      | Bop_max -> ps "max"

    and pp_func = function
      | Fn_sum -> ps "SUM"
      | Fn_prod -> ps "PROD"
      | Fn_max -> ps "MAX"
      | Fn_min -> ps "MIN"
  end
end
