type typ =
  | Tint
  | Tarrow of typ * typ
  | Tproduct of typ * typ
  | Tvar of tvar
and tvar =
  { id: int; mutable def: typ option }

module V = struct
  type t = tvar
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end

let rec head t = match t with
  | Tvar v ->
    begin match v.def with
      | Some nt -> head nt
      | _ -> t 
    end
  | _ -> t

let rec canon t = match t with
  | Tint -> Tint
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tproduct (t1, t2) -> Tproduct (canon t1, canon t2)
  | Tvar v ->
    begin match v.def with
      | Some nt -> canon nt
      | _ -> t
    end

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (head ta == ta);
  assert (head tb == tb);
  let ty = Tarrow (ta, tb) in
  a.def <- Some tb;
  assert (head ta == tb);
  assert (head tb == tb);
  b.def <- Some Tint;
  assert (head ta = Tint);
  assert (head tb = Tint);
  assert (canon ta = Tint);
  assert (canon tb = Tint);
  assert (canon ty = Tarrow (Tint, Tint))

exception UnificationFailure of typ * typ

let unification_error t1 t2 = raise (UnificationFailure (canon t1, canon t2))

let rec occur vt t = match head t with
  | Tint -> false
  | Tarrow (t1, t2) -> occur vt t1 || occur vt t2
  | Tproduct (t1, t2) -> occur vt t1 || occur vt t2
  | Tvar v -> V.equal v vt 

let rec unify t1 t2 = match head t1, head t2 with
  | Tint, Tint -> ()
  | Tarrow (t11, t12), Tarrow (t21, t22) -> unify t11 t21; unify t12 t22
  | Tproduct (t11, t12), Tproduct (t21, t22) -> unify t11 t21; unify t12 t22
  | Tvar v1, Tvar v2 -> v2.def <- Some (Tvar v1)
  | Tvar v, t when not (occur v t) -> v.def <- Some t
  | t, Tvar v -> unify (Tvar v) t
  | _ -> unification_error t1 t2

let () =
  let a = V.create () in
  let b = V.create () in
  let ta = Tvar a in
  let tb = Tvar b in
  assert (occur a ta);
  assert (occur b tb);
  assert (not (occur a tb));
  let ty = Tarrow (ta, tb) in
  assert (occur a ty);
  assert (occur b ty);
  (* unifie 'a -> 'b et int -> int *)
  unify ty (Tarrow (Tint, Tint));
  assert (canon ta = Tint);
  assert (canon ty = Tarrow (Tint, Tint));
  (* unifie 'c et int -> int *)
  let c = V.create () in
  let tc = Tvar c in
  unify tc ty;
  assert (canon tc = Tarrow (Tint, Tint));
  (* tests rajoutes par moi *)
  let c = V.create () in
  unify (Tvar c) (Tvar c)

let cant_unify ty1 ty2 =
  try let _ = unify ty1 ty2 in false with UnificationFailure _ -> true

let () =
  assert (cant_unify Tint (Tarrow (Tint, Tint)));
  assert (cant_unify Tint (Tproduct (Tint, Tint)));
  let a = V.create () in
  let ta = Tvar a in
  unify ta (Tarrow (Tint, Tint));
  assert (cant_unify ta Tint)

module Vset = Set.Make(V)

let rec fvars t = match head t with
  | Tint -> Vset.empty
  | Tarrow (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tproduct (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tvar v -> Vset.singleton v

let () =
  assert (Vset.is_empty (fvars (Tarrow (Tint, Tint))));
  let a = V.create () in
  let ta = Tvar a in
  let ty = Tarrow (ta, ta) in
  assert (Vset.equal (fvars ty) (Vset.singleton a));
  unify ty (Tarrow (Tint, Tint));
  assert (Vset.is_empty (fvars ty))

type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let add id t env =
  { bindings = Smap.add id { vars = Vset.empty; typ = t } env.bindings;
    fvars = Vset.union env.fvars (fvars t) } 

let add_gen id t env =
  let update fl =
    Vset.fold (fun x a -> Vset.union (fvars (Tvar x)) a) fl Vset.empty in
  { bindings = Smap.add id { vars = Vset.diff (fvars t) (update env.fvars); typ = t } env.bindings;
    fvars = env.fvars } 

(* /!\ note *)

module Vmap = Map.Make(V)

let find id env = 
  let sx = Smap.find id env.bindings in
  let map = Vset.fold (fun x a -> Vmap.add x (V.create ()) a) sx.vars Vmap.empty in
  let rec transform t = match head t with
    | Tint -> Tint
    | Tarrow (t1, t2) -> Tarrow (transform t1, transform t2)
    | Tproduct (t1, t2) -> Tproduct (transform t1, transform t2)
    | Tvar tv -> try Tvar (Vmap.find tv map) with Not_found -> t
  in
  transform sx.typ

type expression =
  | Var of string
  | Const of int
  | Op of string
  | Fun of string * expression
  | App of expression * expression
  | Pair of expression * expression
  | Let of string * expression * expression

let rec w env = function
  | Var id -> find id env
  | Const n -> Tint
  | Op "+" -> Tarrow (Tproduct (Tint, Tint), Tint)
  | Fun (id, exp) ->
    let a = Tvar (V.create ()) in
    Tarrow (a, w (add id a env) exp)
  | App (exp1, exp2) ->
    let t1 = w env exp1 in
    let t2 = w env exp2 in
    let a = Tvar (V.create ()) in
    unify t1 (Tarrow (t2, a));
    a
  | Pair (exp1, exp2) ->
    Tproduct (w env exp1, w env exp2)
  | Let (id, exp1, exp2) ->
    let t1 = w env exp1 in
    w (add_gen id t1 env) exp2
  | _ -> failwith "this expression is not known"

let typeof e = canon (w empty e)

(* 1 : int *)
let () = assert (typeof (Const 1) = Tint)

(* fun x -> x : 'a -> 'a *)
let () = assert (match typeof (Fun ("x", Var "x")) with
  | Tarrow (Tvar v1, Tvar v2) -> V.equal v1 v2
  | _ -> false)

(* fun x -> x+1 : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Const 1))))
                 = Tarrow (Tint, Tint))

(* fun x -> x+x : int -> int *)
let () = assert (typeof (Fun ("x", App (Op "+", Pair (Var "x", Var "x"))))
                 = Tarrow (Tint, Tint))

(* let x = 1 in x+x : int *)
let () =
  assert (typeof (Let ("x", Const 1, App (Op "+", Pair (Var "x", Var "x"))))
          = Tint)

(* let id = fun x -> x in id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"), App (Var "id", Const 1)))
          = Tint)

(* let id = fun x -> x in id id 1 *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       App (App (Var "id", Var "id"), Const 1)))
          = Tint)

(* let id = fun x -> x in (id 1, id (1,2)) : int * (int * int) *)
let () =
  assert (typeof (Let ("id", Fun ("x", Var "x"),
		       Pair (App (Var "id", Const 1),
			     App (Var "id", Pair (Const 1, Const 2)))))
          = Tproduct (Tint, Tproduct (Tint, Tint)))

(* app = fun f x -> let y = f x in y : ('a -> 'b) -> 'a -> 'b *)
let () =
  let ty =
    typeof (Fun ("f", Fun ("x", Let ("y", App (Var "f", Var "x"), Var "y"))))
  in
  assert (match ty with
    | Tarrow (Tarrow (Tvar v1, Tvar v2), Tarrow (Tvar v3, Tvar v4)) ->
        V.equal v1 v3 && V.equal v2 v4
    | _ -> false)

(* négatifs *)

let cant_type e =
  try let _ = typeof e in false with UnificationFailure _ -> true

(* 1 2 *)
let () = assert (cant_type (App (Const 1, Const 2)))

(* fun x -> x x *)
let () = assert (cant_type (Fun ("x", App (Var "x", Var "x"))))

(* (fun f -> +(f 1)) (fun x -> x) *)
let () = assert (cant_type
                   (App (Fun ("f", App (Op "+", App (Var "f", Const 1))),
                         Fun ("x", Var "x"))))

(* fun x -> (x 1, x (1,2)) *)
let () = assert (cant_type
                   (Fun ("x", Pair (App (Var "x", Const 1),
                                    App (Var "x", Pair (Const 1, Const 2))))))

(* fun x -> let z = x in (z 1, z (1,2)) *)
let () = assert (cant_type
                   (Fun ("x",
                         Let ("z", Var "x",
                              Pair (App (Var "z", Const 1),
                                    App (Var "z", Pair (Const 1, Const 2)))))))

(* let distr_pair = fun f -> (f 1, f (1,2)) in distr_pair (fun x -> x) *)
let () =
  assert (cant_type
            (Let ("distr_pair",
                  Fun ("f", Pair (App (Var "f", Const 1),
                                  App (Var "f", Pair (Const 1, Const 2)))),
                  App (Var "distr_pair", (Fun ("x", Var "x"))))))
