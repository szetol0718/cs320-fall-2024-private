let last_function_standing funcs start pred =
  let rec step survivors current_value =
    match survivors with
    | [] -> None  (* No functions survive *)
    | [f] -> Some f  (* Only one function survives *)
    | _ ->  (* More than one function, continue the step process *)
        let new_survivors = 
          List.filter (fun f -> not (pred (f current_value))) survivors
        in
        if new_survivors = [] then None  (* No survivors left, return None *)
        else step new_survivors (List.fold_left (fun acc f -> f acc) current_value survivors)
  in
  step funcs start


  