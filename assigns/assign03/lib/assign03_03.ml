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
let rec collapse_helper target_height current_height t =
  match t with
  | Leaf _ -> t
  | Node children ->
      if current_height = target_height - 1 then
        (* Replace the children of the current node with terminal elements *)
        Node (flatten (List.map collect_terminals children))
      else
        (* Continue traversing the tree *)
        Node (List.map (collapse_helper target_height (current_height + 1)) children)

(* Main collapse function *)
let collapse h t =
  if h <= 0 then
    t
  else
    collapse_helper h 0 t





