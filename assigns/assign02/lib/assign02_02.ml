(* Define the matrix type *)
type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

(* Helper function to split a list into two parts: first n elements and the rest *)
let rec split_at n lst =
  match (n, lst) with
  | (0, _) -> ([], lst)
  | (_, []) -> ([], [])
  | (n, x :: xs) -> 
      let (left, right) = split_at (n - 1) xs in
      (x :: left, right)

(* Helper function to split a list into sublists of length n *)
let rec split_list n lst =
  match lst with
  | [] -> []
  | _ -> let (hd, tl) = split_at n lst in
         hd :: split_list n tl

(* Function to create a matrix *)
let mk_matrix (entries: float list) ((r, c): int * int): matrix =
  (* Split the entries list into r rows, each containing c elements *)
  let rows_list = split_list c entries in
  { entries = rows_list; rows = r; cols = c }