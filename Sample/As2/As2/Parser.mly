%parameter < Ast : Ast.S >

%{

open Printf
open Lexing

%}

/* meta tokens */
%token EOL EOF
%token <char> OTHER_CHAR

/* punctiation, separators */
%token COLON SEMI PIPE BANG
%token DOT COMMA EQUALS

/* grouping, nesting */
%token LPAREN   RPAREN
%token LBRACE   RBRACE
%token LBRACKET RBRACKET

/* math */
%token ADD SUB DIV MUL
%token SUM MAX MIN

/* keywords */
%token NEXT PREV GOTO ROW COL SHEET
%token EXIT HELP SCRAMBLE SCRAMBLE_D SCRAMBLE_1 PRINT
%token REPEAT DO DONE

%token <int> LETTER
%token <int> NAT
%token <Num.num> NUM /* use Ocaml standard library of arbitrary-precision numbers. */

/* See ast.ml */

%start cmd

%type <Ast.cmd> cmd
%type <Ast.formula'> frm

%%

cmd:
| EXIT        { Ast.C_exit }
| HELP        { Ast.C_help }
| imp_cmd DOT { $1 }
;

imp_cmd:
| imp_cmd_ SEMI imp_cmd { Ast.C_seq ( $1, $3 ) }
| imp_cmd_ { $1 }
;

imp_cmd_:
| REPEAT frm DO imp_cmd DONE { Ast.C_repeat ($2, $4) }
| mut_cmd { Ast.C_mut $1 }
| nav_cmd { Ast.C_nav $1 }
| PRINT   { Ast.C_print }
;

mut_cmd:
| EQUALS frm  { Ast.C_set $2 }
| SCRAMBLE    { Ast.C_scramble (Ast.Sf_sparse) }
| SCRAMBLE_D  { Ast.C_scramble (Ast.Sf_dense) }
| SCRAMBLE_1  { Ast.C_scramble (Ast.Sf_one) }
;

nav_cmd:
| NEXT nav_thing { Ast.C_next $2 }
| PREV nav_thing { Ast.C_prev $2 }
| GOTO coord     { Ast.C_goto $2 }
;

nav_thing:
| ROW   { Ast.Nav_row }
| COL   { Ast.Nav_col }
| SHEET { Ast.Nav_sht }
;

num:
| NAT { Num.Int $1 }
| NUM { $1 }
;

local_coord:
| LETTER NAT { ($1, $2) }
;
coord:
| local_coord                 { Ast.Lcl $1 }
| SHEET NAT BANG local_coord  { Ast.Abs ($2, $4) }
|       NAT BANG local_coord  { Ast.Abs ($1, $3) }
;

local_region:
| local_coord COLON local_coord { ($1, $3) }
;
region:
| local_region                { Ast.R_lcl $1 }
| SHEET NAT BANG local_region { Ast.R_abs ($2,$4) }
;

frm:
| frm_term ADD frm { Ast.memo_frm (Ast.F_binop(Ast.Bop_add,$1,$3)) }
| frm_term SUB frm { Ast.memo_frm (Ast.F_binop(Ast.Bop_sub,$1,$3)) }
| frm_term         { $1 }
;
frm_term:
| frm_factor DIV frm_term { Ast.memo_frm (Ast.F_binop(Ast.Bop_div,$1,$3)) }
| frm_factor MUL frm_term { Ast.memo_frm (Ast.F_binop(Ast.Bop_mul,$1,$3)) }
| frm_factor              { $1 }
;
frm_factor:
| const                     { Ast.memo_frm (Ast.F_const $1) }
| coord                     { Ast.memo_frm (Ast.F_coord $1) }
| LPAREN frm RPAREN         { Ast.memo_frm  (Ast.F_paren $2) }
| func LPAREN region RPAREN { Ast.memo_frm (Ast.F_func ($1, $3)) }
;
func:
| SUM { Ast.Fn_sum }
| MAX { Ast.Fn_max }
| MIN { Ast.Fn_min }
;
const:
| num { Ast.Num $1 }
;
