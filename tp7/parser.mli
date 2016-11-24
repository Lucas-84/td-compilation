
(* The type of tokens. *)

type token = 
  | WHITE
  | TURNRIGHT
  | TURNLEFT
  | SUB
  | RPAR
  | REPEAT
  | RED
  | RACC
  | PENUP
  | PENDOWN
  | MUL
  | LPAR
  | LACC
  | INT of (int)
  | IF
  | IDENT of (string)
  | GREEN
  | FORWARD
  | EOF
  | ELSE
  | DIV
  | DEF
  | COMMA
  | COLOR
  | BLUE
  | BLACK
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.program)
