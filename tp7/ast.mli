
(* Syntaxe abstraite pour mini-Turtle

   Note : La syntaxe abstraite ci-dessous contient volontairement moins
   de constructions que la syntaxe concr�te. Il faudra donc traduire
   certaines constructions au moment de l'analyse syntaxique (sucre).
*)

(* expressions enti�res *)

type binop = Add | Sub | Mul | Div

type expr =
  | Econst of int
  | Evar   of string
  | Ebinop of binop * expr * expr

(* instructions *)

type stmt =
  | Spenup
  | Spendown
  | Sforward of expr
  | Sturn    of expr (* tourne � gauche *)
  | Scolor   of Turtle.color
  | Sif      of expr * stmt * stmt
  | Srepeat  of expr * stmt
  | Sblock   of stmt list
  | Scall    of string * expr list

(* d�finition de proc�dure *)

type def = {
  name    : string;
  formals : string list; (* arguments *)
  body    : stmt; }

(* programme *)

type program = {
  defs : def list;
  main : stmt; }



