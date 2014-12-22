{
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with
          Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
          Lexing.pos_bol = pos.Lexing.pos_cnum;
      }

  module Make (Ast : Ast.S) (Parser : module type of Parser.Make (Ast)) = struct
    open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let letters = letter*
let nat = ['0'-'9']+

let hash = ['#']
let nothash = [^ '#' '\n']
let noteol = [^'\n']

(* !! Important:
   when we match this, we must increment the line number
   of the lexbuf record. *)
let eol = ['\n']

let strlit = '"' ( [^'"'] | "\\\"" )* '"'

let charlit = '\'' [^'\'']* '\''

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | eol        { incr_linenum lexbuf ; token lexbuf }
  | '/' '*'    { nested_comment 0 lexbuf }

  (* punctiation, separators *)
  | ':' { COLON }
  | ";" { SEMI }
  | "|" { PIPE }
  | "!" { BANG }
  | '.' { DOT }
  | ','  { COMMA }
  | '=' { EQUALS }

  (* grouping, nesting *)
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '[' { LBRACKET }
  | ']' { RBRACKET }

  (* arith *)
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | "*" { MUL }

  (* primitive functions *)
  | ("SUM"|"sum") { SUM }
  | ("MAX"|"max") { MAX }
  | ("MIN"|"min") { MIN }

  (* Keywords *)
  | ("do"|"DO") { DO }
  | ("done"|"DONE") { DONE }
  | ("repeat"|"REPEAT") { REPEAT }
  | ("NEXT"|"next") { NEXT }
  | ("PREV"|"prev") { PREV }
  | ("GOTO"|"goto") { GOTO }
  | ("ROW"|"row") { ROW }
  | ("COL"|"col") { COL }
  | ("SHEET"|"sheet") { SHEET }
  | ("SCRAMBLE"|"scramble") { SCRAMBLE }
  | ("SCRAMBLED"|"scrambled") { SCRAMBLE_D }
  | ("SCRAMBLE1"|"scramble1") { SCRAMBLE_1 }
  | ("PRINT"|"print") { PRINT }

  (* meta-level commands *)
  | ("EXIT"|"exit"|"QUIT"|"quit") { EXIT }
  | ("HELP"|"help"|"?") { HELP }

  | ['a'-'z' 'A'-'Z'] as a { LETTER ((Char.code (Char.uppercase a)) - (Char.code 'A') + 1)}
  | nat as n { NAT (int_of_string n) }

  (* Line comments *)
  | '-' '-' noteol+ eol { incr_linenum lexbuf ; token lexbuf }
  | '/' '/' noteol+ eol { incr_linenum lexbuf ; token lexbuf }

  (* lastly: *)
  | eof { EXIT }
  | _ as c  { OTHER_CHAR c }


and nested_comment level = parse
  | '/' '*' { nested_comment (level + 1) lexbuf }
  | '*' '/' { if level = 0 then
                token lexbuf
              else
                nested_comment (level-1) lexbuf }
  | eol { incr_linenum lexbuf ; nested_comment level lexbuf }
  | _   { nested_comment level lexbuf }

{
  end
}