(* Define the types *)
type ident = string
type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

(* The function to lookup variable types in the context *)
let rec lookup ctxt x =
  match ctxt with
  | [] -> None
  | (y, ty) :: rest -> if x = y then Some ty else lookup rest x

(* The type inference function *)
let rec type_of ctxt e =
  match e with
  | Var x -> lookup ctxt x
  | Fun (x, arg_ty, body) ->
      (* Extend the context with the argument type and check the body *)
      (match type_of ((x, arg_ty) :: ctxt) body with
       | Some body_ty -> Some (Arr (arg_ty, body_ty))
       | None -> None)
  | App (e1, e2) ->
      (* Infer types for e1 and e2 *)
      (match type_of ctxt e1, type_of ctxt e2 with
       | Some (Arr (arg_ty, res_ty)), Some e2_ty ->
           if arg_ty = e2_ty then Some res_ty else None
       | _ -> None)
