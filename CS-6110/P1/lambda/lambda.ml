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
  failwith "This is part (a)!"

(* Substitute `v` for `x` in `e`, avoiding capture. *)
let rec subst (e : expr) (v : expr) (x : id) : expr =
  failwith "This is part (b)!"

(* Apply one call-by-value beta-reduction step to `e`. If `e` cannot be
 * reduced under CBV, the function can throw an error. *)
let rec cbv_step (e : expr) : expr =
  failwith "This is part (c)!"
