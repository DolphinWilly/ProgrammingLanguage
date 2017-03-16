(* An interpreter for IMP based on its *large-step* semantics. This is the
 * only file you need to modify to complete Homework 2's programming
 * problem. *)

open Ast
open State

(* Evaluate an arithmetic expression in a state. *)
let rec eval_a (a : aexp) (s : state) : int =
  match a with
  | Var(x) -> lookup s x
  | Number(n) -> n
  | Plus(a1,a2) -> eval_a a1 s + eval_a a2 s
  | Minus(a1,a2) -> eval_a a1 s - eval_a a2 s
  | Times(a1,a2) -> eval_a a1 s * eval_a a2 s
  | Div(a1,a2) ->
     if eval_a a2 s = 0 then begin
         print_string  "Divide by zero error"; exit 1;
       end
     else
       eval_a a1 s / eval_a a2 s
  | Mod(a1,a2) ->
     if eval_a a2 s = 0 then begin
        print_string "Mod zero error"; exit 1;
       end
     else
       eval_a a1 s mod eval_a a2 s
  | Input -> print_string "? "; read_int()



(* Evaluate a Boolean expression in a state. *)
let rec eval_b (b : bexp) (s : state) : bool =
  match b with
  | Eq(a1,a2) -> eval_a a1 s = eval_a a2 s
  | Leq(a1,a2) -> eval_a a1 s <= eval_a a2 s
  | Lt(a1,a2) -> eval_a a1 s < eval_a a2 s
  | Not(b) -> not(eval_b b s)
  | And(b1,b2) -> eval_b b1 s && eval_b b2 s
  | Or(b1,b2) -> eval_b b1 s || eval_b b2 s
  | True -> true
  | False -> false

(* Evaluate a command in a state. *)
let rec eval_c (c : com) (s : state) : state =
  match c with
  | While(b,c) -> if eval_b b s then eval_c (Comp(c, While(b,c))) s else eval_c Skip s
  | For (a,c) -> let n =  eval_a a s in
                  if n <= 0 then s
                  else eval_c (Comp (c, For (Number (n - 1), c))) s
  | Cond (b,c1,c2) -> if eval_b b s then eval_c c1 s else eval_c c2 s
  | Comp (c1,c2) -> eval_c c2 (eval_c c1 s)
  | Assg (x,a) ->
     let n = eval_a a s in
     rebind s x n
  | Print (a) ->
     let value = eval_a a s in begin
         Printf.printf "%d\n" value;
         s
         end
  | Skip -> s
