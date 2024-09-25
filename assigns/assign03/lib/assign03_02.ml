(* Helper function to get the first n elements of a list *)
let rec first_n n lst =
  match lst with
  | [] -> []
  | _ when n <= 0 -> []
  | x :: xs -> x :: first_n (n - 1) xs

(* Function to compute the generalized Fibonacci sequence in a tail-recursive manner *)
let gen_fib (l: int list) (k: int) : int =
  let len_l = List.length l in

  (* Tail-recursive helper function to compute the generalized Fibonacci sequence *)
  let rec aux n acc =
    if n < len_l then
      (* If n is within the starting values, return the corresponding element *)
      List.nth l n
    else
      (* Compute the sum of the last len_l elements in acc *)
      let next_value = List.fold_left (+) 0 acc in
      (* Update acc to keep only the most recent len_l elements *)
      let new_acc = (next_value :: acc) |> List.rev |> first_n len_l |> List.rev in
      aux (n - 1) new_acc
  in
  aux k (List.rev l)
