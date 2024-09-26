(* Helper function to check if the list is valid *)
let is_valid l =
  let rec aux prev_sign = function
    | [] -> true
    | 0 :: rest -> (
        match prev_sign, rest with
        | None, _ -> false
        | Some s, x :: _ when x <> 0 && (x * s) < 0 -> aux None rest
        | _ -> false)
    | x :: rest ->
        let sign = if x > 0 then 1 else -1 in
        match prev_sign with
        | None -> aux (Some sign) rest
        | Some s when s = sign -> aux prev_sign rest
        | _ -> false
  in
  aux None l

(* Helper function to group a valid list *)
let group_helper l =
  let rec aux current_group acc = function
    | [] -> List.rev (List.rev current_group :: acc)
    | 0 :: rest -> if current_group = [] then aux [] acc rest else aux [] (List.rev current_group :: acc) rest
    | x :: rest -> aux (x :: current_group) acc rest
  in
  aux [] [] l

(* Main grouping function *)
let group l =
  if is_valid l then
    Some (group_helper l)
  else
    None
;;

(* Example usage *)
let l1 = [1; 2; 3; 0; -1; -2; -3; 0; 1];;
let l1_grouping = group l1;; (* Expected output: Some [[1; 2; 3]; [-1; -2; -3]; [1]] *)
