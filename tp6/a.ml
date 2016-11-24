type terminal = string

type non_terminal = string

type symbol =
  | Terminal of terminal
  | NonTerminal of non_terminal

type production = symbol list

type rule = non_terminal * production

type grammar = {
  start : non_terminal;
  rules : rule list;
}

(* Calcul de point fixe *)
let rec fixpoint f x =
  let y, b = f x in
  if b then fixpoint f y
  else x

(* Calcul des nuls *)
module Ntset = Set.Make(String)
type nulls = Ntset.t

let is_null_production nulls prod =
  let null_reachable_from_symbol x = match x with
    | Terminal _ -> false
    | NonTerminal nt -> Ntset.mem nt nulls
  in
  List.for_all null_reachable_from_symbol prod

let null g =
  let step nulls =
    List.fold_left (fun ((new_nulls, changed) as acc) (nt, prod) -> 
      if not (Ntset.mem nt new_nulls) &&
         is_null_production new_nulls prod then
        (Ntset.add nt new_nulls, true)
      else acc) (nulls, false) g.rules 
  in
  fixpoint step Ntset.empty

(* Calcul des first *)
module Ntmap = Map.Make(String)
module Tset  = Set.Make(String)
type firsts = Tset.t Ntmap.t

let empty_map g =
  List.fold_left (fun m (nt, prod) ->
    Ntmap.add nt Tset.empty m) Ntmap.empty g.rules

let rec first_production_step nulls firsts prod = match prod with
  | [] -> Tset.empty
  | Terminal t :: _ -> Tset.singleton t
  | NonTerminal x :: beta when Ntset.mem x nulls ->
    Tset.union (Ntmap.find x firsts) (first_production_step nulls firsts beta)
  | NonTerminal x :: _ -> Ntmap.find x firsts

let first g nulls =
  let step firsts =
    List.fold_left (fun ((new_firsts, changed) as acc) (nt, prod) ->
      let old_set = Ntmap.find nt new_firsts in
      let new_set = first_production_step nulls new_firsts prod in
      if Tset.subset new_set old_set then
        acc
      else
        (Ntmap.add nt (Tset.union old_set new_set) new_firsts, true))
      (firsts, false) g.rules
  in
  fixpoint step (empty_map g)

(* Calcul des suivants *)
type follows = Tset.t Ntmap.t

let follow g nulls firsts =
  let update ((follows, b) as acc) nt follow_nt =
    (** 
     * Met a jour la table follows avec nt -> follow_nt et remplace b
     * par true si la table a ete modifiee
     *)
    let old_set = Ntmap.find nt follows in
    if Tset.subset follow_nt old_set then acc
    else
      (Ntmap.add nt (Tset.union old_set follow_nt) follows, true)
  in
  let rec update_prod ((follows, b) as acc) y prod =
    (**
     * Examine la production y -> prod de la grammaire et met a jour le couple
     * (follows, b) pour tout non-terminal X de prod
     *)
    match prod with
      | [] -> acc 
      | Terminal _ :: t -> update_prod acc y t
      | NonTerminal x :: beta ->
        let acc' =
          update acc x (first_production_step nulls firsts beta) in
        let acc'' = 
          if is_null_production nulls beta then
            update acc' x (Ntmap.find y follows)
          else
            acc'
        in
        update_prod acc'' y beta 
  in
  let step follows =
    List.fold_left
      (fun acc (nt, p) -> update_prod acc nt p)
      (follows, false) g.rules
  in
  fixpoint step (empty_map g)

(* Construction de la table d'analyse LL(1) *)
module Tmap = Map.Make(String)
module Pset = Set.Make(struct type t = production let compare = compare end)
type expansion_table = Pset.t Tmap.t Ntmap.t

let add_entry table nt t prod =
  let line = try Ntmap.find nt table with Not_found -> Tmap.empty in
  let cell = try Tmap.find t line with Not_found -> Pset.empty in
  Ntmap.add nt (Tmap.add t (Pset.add prod cell) line) table
    
let expansions g =
  let nulls = null g in
  let firsts = first g nulls in
  let follows = follow g nulls firsts in
  List.fold_left (fun table (nt, prod) ->
    let table' =
      Tset.fold (fun a tmp -> add_entry tmp nt a prod)
                (first_production_step nulls firsts prod) table
    in
    if is_null_production nulls prod then
      Tset.fold (fun a tmp -> add_entry tmp nt a prod)
                (Ntmap.find nt follows) table'
    else
      table'
    ) Ntmap.empty g.rules
    
    
(* Tests *)
let g_arith =
  { start = "S'";
    rules = [ "S'", [ NonTerminal "E"; Terminal "#" ];
              "E",  [ NonTerminal "T"; NonTerminal "E'"; ];
              "E'", [ Terminal "+"; NonTerminal "T"; NonTerminal "E'"; ];
              "E'", [ ];
              "T",  [ NonTerminal "F"; NonTerminal "T'"; ];
              "T'", [ Terminal "*"; NonTerminal "F"; NonTerminal "T'"; ];
              "T'", [ ];
              "F",  [ Terminal "("; NonTerminal "E"; Terminal ")"; ];
              "F",  [ Terminal "int" ]; ] }

let g1 = {
  start = "S'";
  rules = ["S'", [NonTerminal "S"; Terminal "#"];
	   "S", [];
	   "S", [Terminal "a"; NonTerminal "A"; NonTerminal "S"];
	   "S", [Terminal "b"; NonTerminal "B"; NonTerminal "S"];
	   "A", [Terminal "a"; NonTerminal "A"; NonTerminal "A"];
	   "A", [Terminal "b"];
	   "B", [Terminal "b"; NonTerminal "B"; NonTerminal "B"];
	   "B", [Terminal "a"];
	  ]
}

let table1 = expansions g1

let pp_symbol fmt = function
  | Terminal s -> Format.fprintf fmt "\"%s\"" s
  | NonTerminal s -> Format.fprintf fmt "%s" s

let rec pp_production fmt = function
  | [] -> ()
  | [x] -> pp_symbol fmt x
  | x :: l -> Format.fprintf fmt "%a %a" pp_symbol x pp_production l

let pp_table fmt t =
  let print_entry c p =
    Format.fprintf fmt "  %s: @[%a@]@\n" c pp_production p in
  let print_row nt m =
       Format.fprintf fmt "@[Expansions for %s:@\n" nt;
       Tmap.iter (fun c rs -> Pset.iter (print_entry c) rs) m;
       Format.fprintf fmt "@]" in
  Ntmap.iter print_row t

(**
let () = Format.printf "%a@." pp_table table1
*)
let table_arith = expansions g_arith
(**
let () = Format.printf "%a@." pp_table table_arith
*)

(* Caracterisation LL(1) *)
let is_ll1 table =
  Ntmap.fold (fun _ line ans -> ans &&
    (Tmap.fold
      (fun _ cell ans -> ans && (Pset.cardinal cell <= 1)) line true))
    table true

let () = assert (is_ll1 table1)
let () = assert (is_ll1 table_arith)

let g_gram =
  { start = "S'";
    rules = [ "S'", [ NonTerminal "S"; Terminal "#" ];
              "S",  [ NonTerminal "R" ];
              "S",  [ NonTerminal "R"; Terminal ";"; NonTerminal "S" ];
              "R",  [ Terminal "ident"; Terminal "::="; NonTerminal "P"];
              "P",  [ NonTerminal "W" ];
              "P",  [ NonTerminal "W"; Terminal "|"; NonTerminal "P" ];
              "W",  [ ];
              "W",  [ NonTerminal "C"; NonTerminal "W";];
              "C",  [ Terminal "ident"];
              "C",  [ Terminal "string"];
            ] }

let table_gram = expansions g_gram
(*let () = Format.printf "%a@." pp_table table_gram*)
let () = assert (not (is_ll1 table_gram))

(* Reconnaissance d'un mot *)

let analyze nt table word =
  let rec solve = function
    | [], [] -> true
    | Terminal a :: ta, c :: tc when a = c ->
      solve (ta, tc)
    | NonTerminal x :: tx, ((c :: _) as w) ->
      let beta = Pset.choose (Tmap.find c (Ntmap.find x table)) in
      solve (beta @ tx, w)
    | _ -> false
  in
  try solve ([NonTerminal nt], word @ ["#"]) with Not_found -> false

let explode s =
  let n = String.length s in
  let rec make i = if i = n then [] else String.make 1 s.[i] :: make (i+1) in
  make 0

let test1 s = analyze g1.start (expansions g1) (explode s)

let () = assert (test1 "")
let () = assert (test1 "ab")
let () = assert (test1 "ba")
let () = assert (test1 "abab")
let () = assert (test1 "aaabbb")
let () = assert (test1 "aaabababbbababab")

let () = assert (not (test1 "a"))
let () = assert (not (test1 "b"))
let () = assert (not (test1 "aab"))
let () = assert (not (test1 "aaabbba"))
let () = assert (not (test1 "aaabbbaabab"))
let () = assert (not (test1 "aaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbbbbb"))
