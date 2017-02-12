(* An interpreter for the lambda-calculus. *)

open Printf
open Util
open Ast

(* Check whether an expression is a value. A value is a fully evaluated
 * expression. *)
let is_value (e : expr) : bool =
  match e with
  | Fun _ -> true
  | Var x -> failwith ("Unbound variable " ^ x)
  | _ -> false

(* Check whether a term is closed. *)
let is_closed (e : expr) : bool = HashSet.size (fv e) = 0

(* Translate from an expression of type `expr_s`, with syntactic sugar, to
   a sugar-free expression of type `expr`. Functions with zero arguments are
   not allowed; you can just throw an error if one is provided. *)
let rec translate (e : expr_s) : expr =
  match e with
  | Var_s x -> Var x
  | Fun_s ([],e_s) -> failwith "empty arguments"
  | Fun_s ([x],e_s) -> Fun (x, translate e_s)
  | Fun_s (h::t,e_s) -> Fun (h,translate (Fun_s (t, e_s)))
  | Let_s (x,e1_s,e2_s) -> App (Fun (x,translate e2_s), translate e1_s)
  | App_s (e1_s, e2_s) -> App (translate e1_s, translate e2_s)

(* Substitute `v` for `x` in `e`, avoiding capture. *)
let rec subst (e : expr) (v : expr) (x : id) : expr =
  match e with
  | Var y -> if y = x then v else e
  | App(e1,e2) -> App ((subst e1 v x),(subst e2 v x))
  | Fun (y,e') -> if y = x then e
                  else if not (HashSet.mem (fv v) y) then Fun (y,subst e' v x)
                  else let z = fresh (App (App (e',v), App (Var x,Var y)))in
                    Fun (z,subst (subst e' (Var z) y) v x)

(* Apply one call-by-value beta-reduction step to `e`. If `e` cannot be
 * reduced under CBV, the function can throw an error. *)
let rec cbv_step (e : expr) : expr =
  match e with
  | Var x -> failwith ("Unbound variable" ^ x)
  | Fun _ -> failwith "Cannot step under CBV, already a value"
  | App (Fun (x,e1), (Fun _ as e2)) -> subst e1 e2 x
  | App (Fun (x,e1), e2) -> App (Fun (x,e1), cbv_step e2)
  | App (e1, e2) -> App (cbv_step e1, e2)
