open Utils

let rec type_of (e : expr) : ty option =
  match e with
  | Num _ -> Some TInt  (* A number is always of type TInt *)
  | Add (e1, e2) -> 
      (* Both operands should be TInt for addition *)
      (match type_of e1, type_of e2 with
      | Some TInt, Some TInt -> Some TInt
      | _ -> None)
  | Lt (e1, e2) ->
      (* Both operands should be TInt for comparison *)
      (match type_of e1, type_of e2 with
      | Some TInt, Some TInt -> Some TBool
      | _ -> None)
  | Ite (e1, e2, e3) ->
      (* The first expression should be TBool, and the other two should have the same type *)
      (match type_of e1, type_of e2, type_of e3 with
      | Some TBool, Some t2, Some t3 when t2 = t3 -> Some t2
      | _ -> None)
