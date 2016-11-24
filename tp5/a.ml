type ichar = char * int

type regexp =
  | Epsilon
  | Character of ichar
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp

let rec null = function
  | Epsilon -> true
  | Character _ -> false
  | Union (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2
  | Star _ -> true

module Cset = Set.Make(struct type t = ichar let compare = compare end)

let rec first = function
  | Epsilon -> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1, r2) -> Cset.union (first r1) (first r2)
  | Concat (r1, r2) when null r1 -> Cset.union (first r1) (first r2)
  | Concat (r1, _) -> first r1
  | Star r -> first r

let rec last = function
  | Epsilon -> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1, r2) -> Cset.union (last r1) (last r2)
  | Concat (r1, r2) when null r2 -> Cset.union (last r1) (last r2)
  | Concat (_, r2) -> last r2
  | Star r -> last r

let rec follow c = function
  | Epsilon | Character _ -> Cset.empty
  | Union (r1, r2) -> Cset.union (follow c r1) (follow c r2)
  | Concat (r1, r2) ->
    let between = if Cset.mem c (last r1) then first r2 else Cset.empty in
    Cset.union (follow c r1) (Cset.union (follow c r2) between)
  | Star r ->
    let between = if Cset.mem c (last r) then first r else Cset.empty in
    Cset.union between (follow c r)

let next_state r q c =
  let add ci ans =
    let (d, _) = ci in
    if c = d then Cset.union (follow ci r) ans
    else ans 
  in
  Cset.fold add q Cset.empty

module Cmap = Map.Make(Char)
module Smap = Map.Make(Cset)

type state = Cset.t

type autom = {
  start : state;
  trans : state Cmap.t Smap.t
}

let eof = ('#', -1)

let make_dfa r =
  let r = Concat (r, Character eof) in
  let trans = ref Smap.empty in
  let rec transitions q =
    if not (Smap.mem q !trans) then begin
      let transforc = Cset.fold (fun (c, _) ans -> Cmap.add c (next_state r q c) ans) q Cmap.empty in
      trans := Smap.add q transforc !trans;
      Cset.iter (fun (c, _) -> transitions (next_state r q c)) q
    end
  in
  let q0 = first r in
  transitions q0;
  { start = q0; trans = !trans }

let fprint_state fmt q =
  Cset.iter (fun (c,i) ->
    if c = '#' then Format.fprintf fmt "# " else Format.fprintf fmt "%c%i " c i) q

  let fprint_transition fmt q c q' =
    Format.fprintf fmt "\"%a\" -> \"%a\" [label=\"%c\"];@\n"
      fprint_state q
      fprint_state q'
      c

  let fprint_autom fmt a =
    Format.fprintf fmt "digraph A {@\n";
    Format.fprintf fmt "  @[\"%a\" [ shape = \"rect\"];@\n" fprint_state a.start;
    Smap.iter
      (fun q t -> Cmap.iter (fun c q' -> fprint_transition fmt q c q') t)
      a.trans;
    Format.fprintf fmt "@]@\n}@."

  let save_autom file a =
    let ch = open_out file in
    Format.fprintf (Format.formatter_of_out_channel ch) "%a" fprint_autom a;
    close_out ch

(*  (a|b)*a(a|b)  *)
let r = Concat (Star (Union (Character ('a', 1), Character ('b', 1))),
                Concat (Character ('a', 2),
                        Union (Character ('a', 3), Character ('b', 2))))
let a = make_dfa r
let () = save_autom "autom.dot" a

let recognize a s =
  let rec recognize_from i q =
    if i = String.length s then Cset.mem eof q
    else recognize_from (i + 1) (Cmap.find s.[i] (Smap.find q a.trans))
  in
  try 
    recognize_from 0 a.start
  with Not_found -> false

let () = assert (recognize a "aa")
let () = assert (recognize a "ab")
let () = assert (recognize a "abababaab")
let () = assert (recognize a "babababab")
let () = assert (recognize a (String.make 1000 'b' ^ "ab"))

let () = assert (not (recognize a ""))
let () = assert (not (recognize a "a"))
let () = assert (not (recognize a "b"))
let () = assert (not (recognize a "ba"))
let () = assert (not (recognize a "aba"))
let () = assert (not (recognize a "abababaaba"))

let r = Star (Union (Star (Character ('a', 1)),
              Concat (Character ('b', 1),
                      Concat (Star (Character ('a',2)),
                               Character ('b', 2)))))
let a = make_dfa r
let () = save_autom "autom2.dot" a

let () = assert (recognize a "")
let () = assert (recognize a "bb")
let () = assert (recognize a "aaa")
let () = assert (recognize a "aaabbaaababaaa")
let () = assert (recognize a "bbbbbbbbbbbbbb")
let () = assert (recognize a "bbbbabbbbabbbabbb")

let () = assert (not (recognize a "b"))
let () = assert (not (recognize a "ba"))
let () = assert (not (recognize a "ab"))
let () = assert (not (recognize a "aaabbaaaaabaaa"))
let () = assert (not (recognize a "bbbbbbbbbbbbb"))
let () = assert (not (recognize a "bbbbabbbbabbbabbbb"))
