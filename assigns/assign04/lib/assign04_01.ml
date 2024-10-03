(* Helper function to compute the lifespan of a function f *)
let lifespan f start pred =
  let rec aux s steps =
    if pred s then steps
    else aux (f s) (steps + 1)
  in
  aux start 0

(* Function to find the last function standing *)
let last_function_standing funcs start pred =
  let rec find_max_func funcs current_max_lifespan max_func =
    match funcs with
    | [] -> max_func
    | f :: rest ->
      let lspan = lifespan f start pred in
      if lspan > current_max_lifespan then
        find_max_func rest lspan (Some f)  (* Update max function *)
      else if lspan = current_max_lifespan then
        find_max_func rest current_max_lifespan None  (* Found a tie *)
      else
        find_max_func rest current_max_lifespan max_func
  in
  match funcs with
  | [] -> None  (* No functions to check *)
  | _ -> find_max_func funcs (-1) None

  