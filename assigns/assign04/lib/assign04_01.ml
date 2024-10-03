
let rec compute_lifespan f s pred step =
  if pred s then step - 1 (* If pred is true, return the number of successful steps *)
  else compute_lifespan f (f s) pred (step + 1)


let lifespan f start pred =
  try
    Some (compute_lifespan f start pred 0) (* We start with step count as 0 *)
  with
  | Stack_overflow -> None (* Handle infinite lifespan case gracefully *)


let last_function_standing funcs start pred =
  let rec find_max_func funcs max_lifespan max_func =
    match funcs with
    | [] -> max_func (* Return the current maximum function when list is empty *)
    | f :: rest ->
      match lifespan f start pred with
      | None -> find_max_func rest max_lifespan max_func (* Skip functions with infinite lifespan *)
      | Some lspan ->
        if lspan > max_lifespan then
          find_max_func rest lspan (Some f) (* Update maximum lifespan and function *)
        else if lspan = max_lifespan then
          find_max_func rest max_lifespan None (* Conflict: multiple functions with same lifespan *)
        else
          find_max_func rest max_lifespan max_func
  in
  match funcs with
  | [] -> None (* Return None if the list of functions is empty *)
  | _ -> find_max_func funcs (-1) None
