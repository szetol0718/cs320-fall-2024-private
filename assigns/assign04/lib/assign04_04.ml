type ident = string
type expr' = 
  | True
  | False
  | Num of int
  | Var of ident
  | Let of ident * expr' * expr'
  | Add of expr' * expr'
  | Or of expr' * expr'
  | IfThenElse of expr' * expr' * expr'

type ty' = 
  | Int
  | Bool

type context = (ident * ty') list

(* Helper function to look up a variable's type in the context *)
let rec lookup_type gamma x =
  match gamma with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else lookup_type rest x

(* Type checker function *)
let rec type_of' gamma e =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Var x -> lookup_type gamma x
  | Add (e1, e2) -> (
      match (type_of' gamma e1, type_of' gamma e2) with
      | (Some Int, Some Int) -> Some Int
      | _ -> None
    )
  | Or (e1, e2) -> (
      match (type_of' gamma e1, type_of' gamma e2) with
      | (Some Bool, Some Bool) -> Some Bool
      | _ -> None
    )
  | IfThenElse (e1, e2, e3) -> (
      match (type_of' gamma e1, type_of' gamma e2, type_of' gamma e3) with
      | (Some Bool, Some t2, Some t3) when t2 = t3 -> Some t2
      | _ -> None
    )
  | Let (x, e1, e2) -> (
      match type_of' gamma e1 with
      | Some t1 -> type_of' ((x, t1) :: gamma) e2
      | None -> None
    )