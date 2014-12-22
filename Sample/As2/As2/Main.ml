(* keep track of the module loaded from Args. *)
let module_name = ref "default"

module Make (AA : AdaptonUtil.Signatures.AType) = struct
  module Ast = Ast.Make (AA)
  module Parser = Parser.Make (Ast)
  module Interp = Interp.Make (Ast)
  module Lexer = Lexer.Make (Ast) (Parser)

  exception Error of exn * (int * int * string)
  exception Internal_error

  let ps = print_string

  let help () =
    ps "=========================================================================\n" ;
    ps "AS2 HELP:                                                                \n" ;
    ps "-------------------------------------------------------------------------\n" ;
    ps " Note 1: all keywords, below in quotes, are also valid in all caps       \n" ;
    ps " Note 2: all commands are terminated with a period                       \n" ;
    ps "-------------------------------------------------------------------------\n" ;
(*    ps "                                                                         \n" ; *)
    ps "Command syntax                                                           \n" ;
    ps " cmd ::=                                                                 \n" ;
    ps " | 'help'               -- this help                                     \n" ;
    ps " | 'exit'               -- use excel instead                             \n" ;
(*    ps "                                                                         \n" ; *)
    ps " | = frm                -- set formula of current cell to frm            \n" ;
    ps " | 'print'              -- print the current sheet                       \n" ;
(*    ps "                                                                         \n" ; *)
    ps " | 'goto' coord         -- goto a specific (sheet), row and column       \n" ;
    ps " | 'next' nav-thing     -- next col/row/sheet                            \n" ;
    ps " | 'prev' nav-thing     -- prev col/row/sheet                            \n" ;
(*    ps "                                                                         \n" ; *)
    ps " | cmd1 ; cmd2          -- command sequencing                            \n" ;
    ps " | 'repeat' n 'do' cmd  -- repeat a command n times                      \n" ;
    ps "                                                                         \n" ;
    ps "-------------------------------------------------------------------------\n" ;
    ps "Formula & Coordinate Syntax                                              \n" ;
    ps "                                                                         \n" ;
    ps " Nav. thing   nav-thing ::= 'row' | 'col' | 'sheet'                      \n" ;
    ps "                                                                         \n" ;
    ps " Formulae     frm       ::= func ( reg )                                 \n" ;
    ps "                         | frm binop frm | num | coord | ( frm )         \n" ;
    ps "                                                                         \n" ;
    ps " Functions    func      ::= 'sum' | 'max' | 'min'                        \n" ;
    ps " Binops       binop     ::= + | - | / | *                                \n" ;
    ps " Regions      reg       ::= lr | 'sheet' num ! lr                        \n" ;
    ps " Coordinates  coord     ::= lc | 'sheet' num ! lc                        \n" ;
    ps " Local coord  lc        ::= letters num                                  \n" ;
    ps " Local region lr        ::= lc : lc                                      \n" ;
    ps "-------------------------------------------------------------------------\n" ;
    ()

  let parse_channel : string -> in_channel -> Ast.cmd =
    fun filename channel ->
      let lexbuf = Lexing.from_channel channel in
      let pos = lexbuf.Lexing.lex_curr_p in
      let _ =
        lexbuf.Lexing.lex_curr_p <-
          { pos with
              Lexing.pos_fname = filename ;
              Lexing.pos_lnum = 1 ;
          }
      in
      let _ = Global.set_lexbuf lexbuf in
      let ast : Ast.cmd =
        try Parser.cmd Lexer.token lexbuf
        with
          | exn -> begin
              let curr = lexbuf.Lexing.lex_curr_p in
              let line = curr.Lexing.pos_lnum in
              let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
              let tok = Lexing.lexeme lexbuf in
              raise (Error (exn, (line, cnum, tok)))
            end
      in
      ast

  let parse_string : string -> Ast.cmd =
    fun input ->
      let lexbuf = Lexing.from_string input in
      let pos = lexbuf.Lexing.lex_curr_p in
      let _ =
        lexbuf.Lexing.lex_curr_p <-
          { pos with
              Lexing.pos_fname = "<string>" ;
              Lexing.pos_lnum = 1 ;
          }
      in
      let _ = Global.set_lexbuf lexbuf in
      let ast : Ast.cmd =
        try Parser.cmd Lexer.token lexbuf
        with
          | exn -> begin
              let curr = lexbuf.Lexing.lex_curr_p in
              let line = curr.Lexing.pos_lnum in
              let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
              let tok = Lexing.lexeme lexbuf in
              raise (Error (exn, (line, cnum, tok)))
            end
      in
      ast

  let measure f =
    let module S = AdaptonUtil.Statistics in
    let x, m = S.measure f
    in
    begin
      Printf.printf "time=%f, heap=%d, stack=%d, update=%d, dirty=%d, clean=%d, eval=%d, create=%d, evict=%d, destroy=%d\n"
        m.S.time m.S.heap m.S.stack
        m.S.update m.S.dirty m.S.clean m.S.evaluate m.S.create m.S.evict m.S.destroy ;
      S.fprintf_count_eq stdout ;
      ( x , m )
    end

  let rec eval_cmd' cmd' cur =
    match cmd' with
      | None -> ps "Oops! Try 'help' for reference information.\n" ; cur
      | Some cmd -> eval_cmd cmd cur

  and eval_cmd cmd cur = begin match cmd with
    | Ast.C_print ->
        (* Important: refresh signals to TotalOrder implementation that
           we want to start re-evaluation now; return to "at the
           beginning of time".  This is a no-op otherwise. *)
        if Ast.A.is_incremental then
          Ast.A.refresh ()
        else () ;
        let (sht,_) = Interp.get_pos cur in
        ps "================================================\n" ;
        Interp.print_region (sht,((1,1),(10,10))) (Interp.get_db cur) stdout ;
        ps "================================================\n" ;
        flush stdout ;
        (* XXX/TODO: Flush the Adapton cache here? Anywhere else? *)
        (* XXX/TODO: Or, perhaps we should expose the flush command to the AS2 user?? *)
        cur
    | Ast.C_seq(c1,c2) ->
        let cur = eval_cmd c1 cur in
        eval_cmd c2 cur
    | Ast.C_repeat(f,c) -> begin
        try
          let cnt = Ast.A.force (Interp.eval cur (Ast.A.force f)) in
          let n = match cnt with
            | Ast.Num n -> Num.int_of_num n
            | _ -> invalid_arg "repeat"
          in
          let rec loop i cur =
            if i <= 0 then cur
            else
              loop (i - 1) ( eval_cmd c cur )
          in
          loop n cur
        with
          | _ -> ps "repeat: Oops!\n" ; cur
      end
    | Ast.C_help -> help () ; cur
    | Ast.C_exit -> exit (1)
    | (Ast.C_nav nc) as cmd ->
        ps "read: navigation command: `" ; Ast.Pretty.pp_cmd cmd ; ps "'\n" ;
        (Interp.move nc cur)

    | (Ast.C_mut mc) as cmd ->
        ps "read: mutation command: `" ; Ast.Pretty.pp_cmd cmd ; ps "'\n" ;
        let cur = Interp.write mc cur in
        (* ps ">> " ; ps (Ast.Pretty.string_of_const (Interp.get_val cur)) ; ps "\n" ; *)
        cur
  end

  let repl_handler cmd' cur () =
    ( eval_cmd' cmd' cur )

  (* REPL = Read-Eval-Print Loop *)
  let rec repl cur =
    Printf.printf "= " ;
    Ast.Pretty.pp_formula (Interp.get_frm cur) ;
    Printf.printf "\n"
    ;
    Ast.Pretty.pp_pos (Interp.get_pos cur)
    ;
    Printf.printf "> %!" ;
    let cmd' =
      try
        Some ( parse_channel "<stdin>" stdin )
      with
        | Error (_, (line, col, token)) ->
            ( Printf.eprintf "line %d, character %d: syntax error at %s\n%!"
                (* filename *) line col
                ( if token = "\n"
                  then "newline"
                  else Printf.sprintf "`%s'" token ) ;
              None
            )
    in
    let cur, _ = measure (repl_handler cmd' cur) in
    (repl cur) (* repl is a tail-recursive loop. *)

  let test test_flags n cur =
    let sht_to_demand = n in
    let num_changes = ! Global.num_changes in
    let module S = AdaptonUtil.Statistics in
    let cmd =
      match test_flags with
        | `No_switch ->
            parse_string
            (Printf.sprintf "scrambled; goto %d!a1 ; print ; repeat %d do scramble1 ; print done ."
               sht_to_demand
               num_changes)

        | `Switch ->
            parse_string
              (Printf.sprintf "scrambled; goto %d!a1 ; print ; repeat %d do scramble1 ; goto %d!a1 ; print ; goto %d!a1 ; print done ."
                 sht_to_demand
                 num_changes
                 (sht_to_demand / 2)
                 sht_to_demand
              )
    in
    let _, m = measure (fun _ -> eval_cmd cmd cur) in
    let out = open_out_gen [Open_creat;Open_append] 0 (!Global.stats_out) in
    Printf.fprintf out
      "%d, %s, %s, %s, %d, %d, %d, %d, %f, %d, %d, %d, %d, %d, %d, %d, %d, %d, %d\n%!"
      (* program args *)
      !Global.random_init
      !module_name
      !Global.func_type
      !Global.func_arity
      (match !Global.eviction_limit with
        | None -> -1
        | Some(l) -> l
      )
      sht_to_demand
      num_changes
      (* log time *)
      (int_of_float (Unix.time())) (* sanity check, resolution in seconds (since 19700101) *)
      (* run data *)
      m.S.time m.S.heap m.S.stack
      m.S.update m.S.dirty m.S.clean m.S.evaluate
      m.S.create m.S.evict
      m.S.destroy_refc m.S.destroy_evict
    ;
    close_out out ;
    ()

  let run () =
    let _ = Printf.printf "Cache Eviction: policy=%s limit=%d\n%!" (!Global.eviction_policy) (match !Global.eviction_limit with None -> -1 | Some(l) -> l) in
    let _ = AA.Eviction.set_limit (!Global.eviction_limit) in
    let db_init  = Interp.empty (!Global.num_sheets,10,10) in
    let cur_init = Interp.cursor (1,(1,1)) db_init in
    match ! Global.func with
      | Global.F_repl -> repl cur_init
      | Global.F_stats_test (n, test_flags) -> test test_flags n cur_init
end

module type S = sig
  val run : unit -> unit
end

let as2_list = List.map begin fun ( name, atype ) ->
    ( name, (module Make ((val atype : AdaptonUtil.Signatures.AType)) : S) )
end AdaptonZoo.All.a_list

let run () =
  let as2 = ref (snd (List.hd as2_list)) in
  let args = Global.args @ [
    ( "--adapton-module", Arg.Symbol ( fst (List.split as2_list), begin fun s -> module_name := s; as2 := List.assoc s as2_list end),
      " use specific Adapton module (default: " ^ (fst (List.hd as2_list)) ^ ")" )
  ] in
  let _ = Arg.parse args
    (fun filename -> invalid_arg "No input files.." )
    "usage: runas2 [options]"
  in
  let module As2 = (val (!as2)) in
  As2.run ()


(* Not in use: FILE processing *)

(* let process_file : string -> Ast.cmd = fun filename -> *)
(*   let _ = Global.cur_filename := filename in *)
(*   let input = *)
(*     if filename = "-" *)
(*     then stdin *)
(*     else open_in filename *)
(*   in *)
(*   let cmd = *)
(*     try parse_channel filename input *)
(*     with *)
(*       | Error (_, (line, col, token)) -> *)
(*           ( Printf.eprintf "File %s, line %d, character %d: syntax error at %s\n%!" *)
(*               filename line col *)
(*               ( if token = "\n" *)
(*                 then "newline" *)
(*                 else Printf.sprintf "`%s'" token ) ; *)
(*             exit (-1) ) *)
(*   in *)
(*   Ast.Pretty.pp_cmd cmd ; *)
(*   cmd *)

(* let run () = *)
(*   if false then *)
(*     let input_files : string list ref = ref [] in *)
(*     if !Global.print_passes then Printf.eprintf "parsing input files...\n%!" ; *)
(*     let _ = Arg.parse Global.args *)
(*       (fun filename -> input_files := filename :: !input_files) *)
(*       "usage: m3pc [options] [input files]" *)
(*     in *)
(*     if !input_files = [] then ( *)
(*       Printf.eprintf "no input files given!\n" ; *)
(*       exit (-1); *)
(*     ); *)
(*     let _ = List.map process_file (List.rev (!input_files)) in *)
(*     (\** TODO -- emit/do something! **\) *)
(*     () *)
(*     ; *)
(*     if !Global.print_passes then *)
(*       Printf.eprintf "done.\n%!" *)
(*     else () *)
