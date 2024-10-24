open Utils

let rec eval (e : expr) : value =
  match e with
  | Num n -> VNum n  (* A number evaluates to itself *)
  | Add (e1, e2) ->
      (* Evaluate both operands and add the results *)
      (match eval e1, eval e2 with
      | VNum v1, VNum v2 -> VNum (v1 + v2)
      | _ -> failwith "Unexpected type in Add")  (* Should not happen if well-typed *)
  | Lt (e1, e2) ->
      (* Evaluate both operands and compare the results *)
      (match eval e1, eval e2 with
      | VNum v1, VNum v2 -> VBool (v1 < v2)
      | _ -> failwith "Unexpected type in Lt")  (* Should not happen if well-typed *)
  | Ite (e1, e2, e3) ->
      (* Evaluate the first expression and choose between the second and third *)
      (match eval e1 with
      | VBool true -> eval e2
      | VBool false -> eval e3
      | _ -> failwith "Unexpected type in Ite")  (* Should not happen if well-typed *)
