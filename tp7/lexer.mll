(* Analyseur lexical pour mini-Turtle *)

{
  open Lexing
  open Parser

  (* exception à lever pour signaler une erreur lexicale *)
  exception Lexing_error of string

  (* fonction à appeler à chaque retour chariot (caractère '\n') *)
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

  let keywords = Hashtbl.create 97
  let () =
    List.iter (fun (x, l) -> Hashtbl.add keywords x l)
      [ "if", IF; "else", ELSE; "def", DEF; "repeat", REPEAT; "penup", PENUP;
        "pendown", PENDOWN; "forward", FORWARD; "turnleft", TURNLEFT;
        "turnright", TURNRIGHT; "color", COLOR; "black", BLACK;
        "white", WHITE; "red", RED; "green", GREEN; "blue", BLUE ]
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0' - '9']
let indent = letter (letter | digit | "_")*
let integer = digit+

rule token = parse
  | [' ' '\t' '\n']+  { token lexbuf }
  | "//"              { comment_line lexbuf }
  | "(*"              { comment_multiline lexbuf }
  | indent as s       { try
                          Hashtbl.find keywords s
                        with Not_found ->  
                          IDENT s }
  | integer as n      { INT (int_of_string n) }
  | '+'               { ADD }
  | '-'               { SUB }
  | '*'               { MUL }
  | '/'               { DIV }
  | '('               { LPAR }
  | ')'               { RPAR }
  | '{'               { LACC }
  | '}'               { RACC }
  | ','               { COMMA }
  | eof               { EOF }
  | _ as c            { failwith ("invalid token : " ^ (String.make 1 c)) }

and comment_line = parse
  | '\n' | eof { token lexbuf }
  | _          { comment_line lexbuf }

and comment_multiline = parse
  | "*)" { token lexbuf }
  | eof  { failwith "commentaire non termine" }
  | _    { comment_multiline lexbuf }
