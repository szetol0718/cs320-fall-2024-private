let gen_fib l k =
  let n = List.length l in
  (* Custom implementation of List.tl *)
  let custom_tl lst =
    match lst with
    | [] -> failwith "Cannot take the tail of an empty list"
    | _ :: t -> t
  in
  (* Helper function to generate the sequence using tail recursion *)
  let rec aux current_list index =
    if index < n then
      List.nth current_list index
    else
      let next_value = List.fold_left (+) 0 current_list in
      aux (custom_tl current_list @ [next_value]) (index - 1)
  in
  aux l k

