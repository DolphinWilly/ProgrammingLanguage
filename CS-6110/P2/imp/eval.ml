(* An interpreter for IMP based on its *large-step* semantics. This is the
 * only file you need to modify to complete Homework 2's programming
 * problem. *)

open Ast
open State

(* Evaluate an arithmetic expression in a state. *)
let rec eval_a (a : aexp) (s : state) : int =
  failwith "Implement me!"

(* Evaluate a Boolean expression in a state. *)
let rec eval_b (b : bexp) (s : state) : bool =
  failwith "Implement me!"

(* Evaluate a command in a state. *)
let rec eval_c (c : com) (s : state) : state =
  failwith "Implement me!"
