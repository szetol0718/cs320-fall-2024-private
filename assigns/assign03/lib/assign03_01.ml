(* Function to sum up values for unique keys in an association list *)
let mk_unique_keys (alst: (string * int) list) : (string * int) list =
  (* Helper function to update the sum for a given key in an association list *)
  let rec update key value acc =
    match acc with
    | [] -> [(key, value)]
    | (k, v) :: t when k = key -> (k, v + value) :: t
    | hd :: t -> hd :: update key value t
  in
  (* Fold over the input list to build the unique keys list *)
  List.fold_left (fun acc (key, value) -> update key value acc) [] alst