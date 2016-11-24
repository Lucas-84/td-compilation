/* Analyseur syntaxique pour mini-Turtle */

%{
  open Ast
  open Turtle

%}

/* Déclaration des tokens */

%token EOF
%token IF ELSE DEF REPEAT
%token ADD SUB MUL DIV
%token COMMA LPAR RPAR LACC RACC
%token FORWARD PENUP PENDOWN TURNLEFT TURNRIGHT COLOR
%token BLACK WHITE RED GREEN BLUE
%token <int> INT
%token <string> IDENT

/* Priorités et associativités des tokens */

%left ADD SUB
%left MUL DIV
%nonassoc IF
%nonassoc ELSE

/* Point d'entrée de la grammaire */
%start prog

/* Type des valeurs renvoyées par l'analyseur syntaxique */
%type <Ast.program> prog

%%

/* Règles de grammaire */

prog:
| d = def*; s = stmt*; EOF { { defs = d; main = Sblock s } }
;

def:
| DEF; n = IDENT; LPAR; l = separated_list(COMMA, IDENT); RPAR; s = stmt
  { {name = n; formals = l; body = s} }
;

stmt:
| FORWARD; e = expr       { Sforward e }
| PENUP                   { Spenup }
| PENDOWN                 { Spendown }
| TURNLEFT; e = expr      { Sturn e }
| TURNRIGHT; e = expr     { Sturn (Ebinop (Sub, Econst 0, e)) }
| COLOR; c = color        { Scolor c } 
| IF; e = expr; s = stmt  { Sif (e, s, Sblock []) }
| IF; e = expr; s1 = stmt; ELSE; s2 = stmt
  { Sif (e, s1, s2) }
| REPEAT; e = expr; s = stmt
  { Srepeat (e, s) }
| LACC; s = stmt*; RACC   { Sblock s }
| n = IDENT; LPAR; l = separated_list(COMMA, expr); RPAR
  { Scall (n, l) }
;

expr:
| n = INT                   { Econst n }
| x = IDENT                 { Evar x }
| e1 = expr; ADD; e2 = expr { Ebinop (Add, e1, e2) }
| e1 = expr; SUB; e2 = expr { Ebinop (Sub, e1, e2) }
| e1 = expr; MUL; e2 = expr { Ebinop (Mul, e1, e2) }
| e1 = expr; DIV; e2 = expr { Ebinop (Div, e1, e2) }
| SUB; e = expr             { Ebinop (Sub, Econst 0, e) }
| LPAR; e = expr; RPAR      { e } 
;

color:
| BLACK { black }
| WHITE { white }
| RED   { red }
| GREEN { green }
| BLUE  { blue }
;
