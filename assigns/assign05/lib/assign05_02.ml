(* Define the type 'a tree *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

(* Tail-recursive sum function using CPS *)
let sum_tr t =
  let rec go t cont =
    match t with
    | Leaf -> cont 0
    | Node (x, l, r) ->
        go l (fun sum_l -> 
          go r (fun sum_r -> 
            cont (x + sum_l + sum_r)))
  in go t (fun x -> x)
