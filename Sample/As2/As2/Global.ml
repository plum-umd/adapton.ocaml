
type stats_test_param = int * [`Switch|`No_switch]
type func =
  | F_repl
  | F_stats_test of stats_test_param

let num_sheets      = ref 20
let func            = ref F_repl
let verbose_errors  = ref false
let print_passes    = ref true
let print_ast_db    = ref false
let stats_out       = ref "as2-stats.out"
let stateless_eval  = ref true
let num_changes     = ref 10
let func_type       = ref "more_fun"
let func_arity      = ref "only_binop"
let eviction_policy = ref "none"
let eviction_limit  = ref None
let random_init     =
  let t = int_of_float(Unix.time()) in
  Random.init t; ref t


let rec args = [
  ("--stateless-eval",    Arg.Set stateless_eval, " use stateless evaluation semantics (default)" ) ;
  ("--stateful-eval",     Arg.Clear stateless_eval, " use stateful evaluation semantics (not the default)" ) ;

  ("--repl",              Arg.Unit begin fun _ -> func := F_repl end, " functionality/mode: read-eval-print-loop (REPL)") ;

  ("--stats-test",        Arg.Int begin fun n -> num_sheets := n; func := F_stats_test (n, `No_switch) end, " functionality/mode: run benchmark script for given number of sheets") ;
(*  ("--stats-test-switch", Arg.Int begin fun n -> num_sheets := n; func := F_stats_test (n, `Switch) end,    " functionality/mode: run a predefined script (that switches), of a given size and record statisitics") ; *)
  ("--num-sheets",        Arg.Int begin fun i -> num_sheets := i end, " set the total number of sheets (default: 20)" ) ;
  ("--num-changes",       Arg.Int begin fun i -> num_changes := i end, " set the number changes in the test script (default: 10)") ;
  ("--stats-out",         Arg.String begin fun s -> stats_out := s end, " write out stats to the given file" ) ;
  ("--func-type",         Arg.Symbol (["only_fun";"more_fun";"all_types";"more_cmp";"only_cmp"], begin fun s -> func_type := s end), " ratio of computing functions(fun) to comparison functions(cmp) when doing binary ops, default: more_fun") ;
  ("--func-arity",        Arg.Symbol (["only_binop";"more_binop";"all_arities";"more_multi";"only_multi"], begin fun s -> func_arity := s end), " ration of binary(binop) to regional(multi) operations, default: only_binop") ;

  ("--random_init",       Arg.Int begin fun i -> random_init := i; Random.init i end, " initialize the Random module's number generator to the given integer (default: self_init" ) ;
  ("--verbose",           Arg.Set verbose_errors, " give verbose (contextual) errors") ;
  ("--ast-db",            Arg.Set print_ast_db, " give verbose debugging information in formulae") ;

  ("--eviction-policy",   Arg.Symbol (["none";"lru";"fifo"], begin fun s -> eviction_policy := s end), " set eviction policy ('none', 'lru', 'fifo')");
  ("--eviction-limit",    Arg.Int (fun l -> eviction_limit := Some(l)), " set eviction limit, measured in nodes.") ;
]

let cur_filename = ref ""

let lexbuf : Lexing.lexbuf ref = ref (Lexing.from_string "")

let set_lexbuf lb = lexbuf := lb
let get_lex_pos _ = (!lexbuf).Lexing.lex_curr_p

module Prov = struct
  type loc = Lexing.position
  type loc_range = loc * loc

  and prov =
    | Synth
    | Root of loc_range
    | Stepped of prov

  let rec sprint_prov prefix = function
    | Synth -> prefix^"synth"
    | Stepped p -> sprint_prov prefix p
    | Root (loc1, loc2) ->
        Printf.sprintf "%sFile \"%s\", line %d, characters %d-%d"
          prefix
          loc1.Lexing.pos_fname
          loc1.Lexing.pos_lnum
          (loc1.Lexing.pos_cnum - loc1.Lexing.pos_bol)
          (loc2.Lexing.pos_cnum - loc2.Lexing.pos_bol)

          (*
    | Subst (n, p1, p2) ->
        Printf.sprintf "%sSubstitution of `%s' at\n%s%sfor original `%s' at\n%s"
          prefix n (sprint_prov (prefix^"  ") p2)
          prefix n (sprint_prov (prefix^"  ") p1) *)
end
