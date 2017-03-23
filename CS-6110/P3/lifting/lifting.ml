(* The lambda-lifting transformation. *)

open Util
open Ast
open State

(* Check whether an expression is in the "target language", i.e., it is
   lambda-free. You can use this function to test whether you have
   successfully converted an expression. *)
let rec is_target_exp (e : exp) : bool =
  match e with
    | Var _ -> true
    | Fun  _ -> false
    | Let _ -> false
    | Letrec _ -> false
    | App (e1, e2) -> is_target_exp e1 && is_target_exp e2
    | Cond (b, e1, e2) -> is_target_exp b && is_target_exp e1 && is_target_exp e2
    | Num _ -> true
    | Plus (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Minus (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Times (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Div (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | Mod (n1, n2) -> is_target_exp n1 && is_target_exp n2
    | And (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Or (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Not b -> is_target_exp b
    | Eq (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Leq (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Lt (b1, b2) -> is_target_exp b1 && is_target_exp b2
    | Bool _ -> true

let rec is_target_fun (e : exp) : bool =
  match e with
    | Fun (_, (Fun _ as e1)) -> is_target_fun e1
    | Fun (_, e1) -> is_target_exp e1
    | _ -> false

let rec is_target_prog (e : exp) : bool =
  match e with
    | Let(_, e1, p2) -> is_target_fun e1 && is_target_prog p2
    | Letrec(_, e1, p2) -> is_target_fun e1 && is_target_prog p2
    | _ -> is_target_exp e

(****helper functions******)
let rec all_apply (e:exp) (l: string list): exp =
  match l with
    | [] -> e
    | h::t -> all_apply (App (e, Var h)) t
(****end of helper functions******)
(* Convert an expression from the source FL to the restricted FL target
   language. e is the expression to convert, and s is a store that contains
   the functions converted *so far*. You will want to add new bindings to s
   and return the expanded mapping along with the converted expression. *)
let convert (e : exp) (s : state) : exp * state =
  let v_gen = Fresh.make (allv e) in
  (**v_gen should be outside the recursion to keep the fresh new!!**)
  let rec convert (e: exp) (s: state): exp * state =
    match e with
      | Var _ -> (e, s)
      | Fun (args, exp) ->
        let (exp', s1) = convert exp s in
        let fvs = HashSet.values (fv (Fun (args, exp'))) in
        let f = Fresh.next v_gen in
        let c = Closure (Fun (fvs@args, exp'), State.make()) in
        let s2 = update s1 f c in
        (all_apply (Var f) fvs, s2)
      | Let ([], e1, e2) -> failwith "invalid Let"
      | Let ([x], e1, e2) -> convert (App (Fun ([x], e2), e1)) s
      | Let (h::t, e1, e2) -> convert (App ((Fun([h],e2)),(Fun(t,e1)))) s
      | Letrec _ -> failwith "not implemented"
      | App (e0, e1) ->
        let (e0', s1) = convert e0 s in
        let (e1', s2) = convert e1 s1 in
        (App (e0', e1'), s2)
      | Cond (b, e1, e2) ->
        let (b', s1) = convert b s in
        let (e1', s2) = convert e1 s1 in
        let (e2', s3) = convert e2 s2 in
        (Cond (b', e1', e2'), s3)
      (***Arithmetic expressions***)
      | Num n  -> Num n, s
      | Plus (l, r)  ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Plus (l', r'), s2
      | Minus (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Minus (l', r'), s2
      | Times (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Times (l', r'), s2
      | Div (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Div (l', r'), s2
      | Mod (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Mod (l', r'), s2
      (***Boolean expression***)
      | And (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        And (l', r'), s2
      | Or (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Or (l', r'), s2
      | Not b -> Not b, s
      | Eq (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Eq (l', r'), s2
      | Leq (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Leq (l', r'), s2
      | Lt (l, r) ->
        let (l', s1) = convert l s in
        let (r', s2) = convert r s1 in
        Lt (l', r'), s2
      | Bool b -> Bool b, s
  in convert e s



  (* Remember: the `Letrec` case is a karma problem (worth zero points). If
     you are not attempting that part, you can just use "failwith" to bail out
     if you encounter a `Letrec` in the AST. *)

(* Convert a source FL expression into a complete, self-contained expression
   in the target version of FL. This function should call `convert` to get the
   new expression and the resulting function bindings. It should then turn all
   the bindings into `let` expressions to compose the complete program. *)
let lift (e : exp) : exp =
  let (e', s) = convert e (State.make()) in
  let rec lifting bl (e: exp) =
    match bl with
      | [] -> e
      | (x, Closure (Fun _ as f, s1)) :: t ->
        if bindings s1 == [] then lifting t (Let ([x], f, e))
        else lifting t (Letrec ([x], f, e))
      | _ -> failwith "error bindings, should bind with function" in
  lifting (bindings s) e'
