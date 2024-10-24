open Utils

let parse (toks : tok list) : expr option =
  let rec aux toks stack =
    match toks, stack with
    | [], [e] -> Some e  (* If all tokens are processed and the stack contains one expression, return it *)
    | [], _ -> None      (* If the stack doesn't have exactly one element, it's not a well-formed program *)
    | TNum n :: rest, _ -> aux rest (Num n :: stack)  (* Push number onto stack *)
    | TAdd :: rest, e2 :: e1 :: stack_tail -> 
        aux rest (Add (e1, e2) :: stack_tail)  (* Pop two, apply Add, push result *)
    | TLt :: rest, e2 :: e1 :: stack_tail -> 
        aux rest (Lt (e1, e2) :: stack_tail)   (* Pop two, apply Lt, push result *)
    | TIte :: rest, e3 :: e2 :: e1 :: stack_tail -> 
        aux rest (Ite (e1, e2, e3) :: stack_tail)  (* Pop three, apply Ite, push result *)
    | _ -> None  (* If we can't match the pattern, it's not a well-formed program *)
  in
  aux toks []
