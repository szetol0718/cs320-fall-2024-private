type tree = 
  | Leaf of int
  | Node of tree list

(* Custom flatten function *)
let rec flatten lst =
  match lst with
  | [] -> []
  | x :: xs -> x @ flatten xs

(* Helper function to collect terminal elements in a tree *)
let rec collect_terminals t =
  match t with
  | Leaf _ -> [t]
  | Node [] -> [t]
  | Node children -> flatten (List.map collect_terminals children)

(* Recursive helper function to collapse a tree to a specified height *)
let rec collapse_helper h current_height t =
  match t with
  | Leaf _ -> t
  | Node children ->
      if current_height = h - 1 then
        Node (collect_terminals t)  (* Collapse at the correct height *)
      else
        Node (List.map (collapse_helper h (current_height + 1)) children)

(* Main collapse function *)
let collapse h t =
  if h <= 0 then
    t
  else
    collapse_helper h 0 t




