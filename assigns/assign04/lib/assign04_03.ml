open Assign04_02

type value = 
  | VNum of int
  | VBool of bool

(* Evaluation function *)
let rec eval e =
  match e with
  | True -> VBool true
  | False -> VBool false
  | Num n -> VNum n
  | Or (e1, e2) -> (
      match (eval e1, eval e2) with
      | (VBool v1, VBool v2) -> VBool (v1 || v2)
      | _ -> failwith "Undefined behavior: Or with non-boolean values"
    )
  | Add (e1, e2) -> (
      match (eval e1, eval e2) with
      | (VNum n1, VNum n2) -> VNum (n1 + n2)
      | _ -> failwith "Undefined behavior: Add with non-integer values"
    )
  | IfThenElse (e1, e2, e3) -> (
      match eval e1 with
      | VBool true -> eval e2
      | VBool false -> eval e3
      | _ -> failwith "Undefined behavior: IfThenElse condition is not a boolean"
    )